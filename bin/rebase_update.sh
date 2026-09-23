#!/usr/bin/env bash
# rebase_update.sh -- rebase the current Spark branch onto its PR's base
# branch, re-add the co-author trailer, clean build-check, force-push TO THE
# FORK.
#
# Both modes lint-scala when Scala/Java moved, then build. The mode picks what
# the build is, whether suites follow, and whether any of it takes a
# /tmp/test-lock slot:
#   --compile-only (default)  clean compile + test:compile, no suites, NO
#                             SLOT -- it runs no tests, so it does not queue
#                             behind the runs that do.
#   --full-test               clean package + test:compile under the lock,
#                             then spark-compile-test-and-retry --fast
#                             --skip-build --base <base> (the suites derived
#                             from the base diff, failures retried
#                             individually).
# spark-presend stays the real pre-push gate -- it also runs check-license,
# lint-python and the structured-logging check, none of which happen here.
#
# Base branch resolution, first hit wins:
#   1. explicit argument:        rebase_update.sh branch-3.5
#   2. the open PR for this branch on apache/spark (via gh)
#   3. a version SUFFIX on the branch name that matches an upstream branch --
#      same grammar as squash-magic.sh: foo-branch-3.5, bar-4.x, baz-4x,
#      qux-4.x-r2. Mid-name versions do not count (ivy2.5.3 is not 2.5).
#   4. master
#
# Rebases onto upstream/<base> (fetched first), falling back to the local
# branch when the fetch fails. Remotes: REMOTE=upstream for the base,
# FORK_REMOTE=origin for the push -- the push target is always explicit
# because some local branches track upstream, and a bare force-push there
# would rewrite apache/spark itself.
#
# --print-base resolves and prints the base branch, then exits. Flags and
# the positional base argument can come in any order.

set -ex

# REBASE_UPDATE_REMOTE first: squash-magic.sh and update-bases.sh read a bare
# $REMOTE too and default it differently (origin, apache-github), so exporting
# REMOTE to steer one of them silently repoints the others in the same shell.
REMOTE="${REBASE_UPDATE_REMOTE:-${REMOTE:-upstream}}"
FORK_REMOTE="${FORK_REMOTE:-origin}"

# Collapse a child's failure to 1. Exit 2 is rebase_update_batch.sh's
# "systematic, stop the whole batch" signal, so it has to mean THIS script
# decided that -- and the children hand out 2 for branch-specific reasons
# (spark-compile-test-and-retry's die() defaults to 2, and BUILD FAILED returns
# 2), which would abandon every remaining branch over one bad base.
run_step() {
  "$@" || { echo "step failed (rc=$?): $*" >&2; exit 1; }
}
# Siblings of this script (resolve through the ~/bin symlink), so moving the
# mydotfiles checkout never stales these. LOCK_HELPER overrides the helper
# used here (--full-test only); spark-compile-test-and-retry resolves its own
# and ignores it.
# Split out: a failing readlink nested inside another $() is swallowed even
# under set -e, leaving SCRIPT_DIR as the invocation cwd.
SELF="$(readlink -f "${BASH_SOURCE[0]}")"
[ -n "$SELF" ] || { echo "cannot resolve own path" >&2; exit 1; }
SCRIPT_DIR="$(cd "$(dirname "$SELF")" && pwd)"
LOCK_HELPER="${LOCK_HELPER:-$SCRIPT_DIR/with-test-lock}"

# Via a variable: cd "" is a silent no-op, so outside a repo the cd would
# succeed and the script would run against the wrong tree.
TOPLEVEL="$(git rev-parse --show-toplevel)"
cd "$TOPLEVEL"
BRANCH="$(git branch --show-current)"
if [ -z "$BRANCH" ]; then
  echo "detached HEAD; check out a branch first" >&2
  exit 1
fi

detect_base_from_pr() {
  command -v gh >/dev/null 2>&1 || return 1
  # No --author @me: it switches --head to prefix matching and a same-named
  # backport branch (foo-4.x-r2) wins over the exact match. Filter exactly.
  local out owner err rc
  # --head matches any fork's branch of that name, so filter on the owner of
  # $FORK_REMOTE too -- otherwise a stranger's PR can hand us its base. An
  # empty owner would match nobody and read as "no PR", so say so instead.
  owner="$(git remote get-url "$FORK_REMOTE" 2>/dev/null \
             | sed -E 's#(.*[:/])([^/]+)/[^/]+(\.git)?$#\2#')"
  # sed prints non-matching input unchanged, so a URL this pattern does not fit
  # (a trailing slash, or an insteadOf alias like hk:spark.git, which
  # `git remote get-url` returns un-rewritten) yields a non-empty non-login
  # that passes an emptiness test, matches no PR, and reads as "no open PR" --
  # the silent master fallback the return 2 below exists to prevent. So check
  # it actually looks like a GitHub login.
  if ! [[ "$owner" =~ ^[A-Za-z0-9]([A-Za-z0-9]|-[A-Za-z0-9]){0,38}$ ]]; then
    echo "cannot read a GitHub owner from remote '$FORK_REMOTE' (got '${owner:-}')." >&2
    echo "pass the base explicitly, or fix the remote URL." >&2
    return 2
  fi
  # Separate query failure from "no open PR": an auth/network error must not
  # read as "this branch targets master" and rebase a backport onto it. Return
  # 2, never `exit`: this runs inside a command substitution, where exit kills
  # only the subshell and the caller falls straight through to master.
  # gh's stderr goes to its own file, not into $out -- it prints update notices
  # on successful calls, and merged they become the "base branch".
  err="$(mktemp)"
  out=$(gh pr list --repo apache/spark --head "$BRANCH" --state open \
      --json baseRefName,headRefName,headRepositoryOwner \
      --jq '.[] | select(.headRefName=="'"$BRANCH"'")
                | select(.headRepositoryOwner.login=="'"$owner"'") | .baseRefName' 2>"$err") \
    && rc=0 || rc=$?
  if [ "$rc" != 0 ]; then
    echo "gh pr list failed, so the PR's base is unknown:" >&2
    cat "$err" >&2
    rm -f "$err"
    return 2
  fi
  rm -f "$err"
  head -n 1 <<<"$out" | grep -v '^$' || return 1
}

detect_base_from_name() {
  # Same suffix grammar as squash-magic.sh's derive_base_from_suffix, so the
  # two scripts never disagree about where a branch was cut from: the version
  # is ANCHORED at the end (an unanchored match reads ivy2.5.3 as 2.5), -Nx is
  # an alias for -N.x, and a trailing -rN is a redone cut, not a version.
  local suffix c name="$BRANCH"
  # Strip squash-magic's own output suffixes first, the way its logical_name()
  # does -- it names its results ${branch}-aok and ${branch}-squashed, so
  # foo-4.0-aok is exactly the branch that exists between a squash and a PR,
  # i.e. when detect_base_from_pr cannot help either. Without this it fell
  # through to master. Loop: -squashed-aok happens.
  while :; do
    case "$name" in
      *-aok)      name="${name%-aok}" ;;
      *-squashed) name="${name%-squashed}" ;;
      *) break ;;
    esac
  done
  [[ "$name" =~ -(branch-)?(master|[0-9]+(\.[0-9]+|\.x)+|[0-9]+x)(-r[0-9]+)?$ ]] \
    || return 1
  suffix="${BASH_REMATCH[2]}"
  [[ "$suffix" =~ ^([0-9]+)x$ ]] && suffix="${BASH_REMATCH[1]}.x"
  if [ "$suffix" = "master" ]; then c="master"; else c="branch-$suffix"; fi
  git rev-parse --verify --quiet "refs/remotes/$REMOTE/$c" >/dev/null || return 1
  echo "$c"
}

PRINT_ONLY=0
FULL_TEST=0
BASE=""
BASE_EXPLICIT=0
while [ $# -ge 1 ]; do
  case "$1" in
    --print-base)   PRINT_ONLY=1 ;;
    --full-test)    FULL_TEST=1 ;;
    --compile-only) FULL_TEST=0 ;;
    -*)
      echo "unknown flag: $1 (want --compile-only, --full-test, or --print-base)" >&2
      exit 2 ;;
    *)
      if [ -n "$BASE" ]; then
        echo "unexpected extra argument: $1" >&2
        exit 2
      fi
      BASE="$1"; BASE_EXPLICIT=1 ;;
  esac
  shift
done

# Best-effort freshness for detection and the rebase target; offline is fine.
git fetch --quiet "$REMOTE" 2>/dev/null || true

if [ -z "$BASE" ]; then
  if BASE=$(detect_base_from_pr); then
    :
  elif [ "$?" = 2 ]; then
    # Systematic (auth, network, misconfigured remote): every branch hits it, so
    # exit 2 and let rebase_update_batch.sh stop rather than mangle the rest.
    echo "pass the base explicitly, e.g. $0 branch-4.0" >&2
    exit 2
  elif BASE=$(detect_base_from_name); then
    :
  else
    BASE="master"
    echo "no PR and no version in the branch name; falling back to master." >&2
    echo "if this branch targets a release branch, pass it: $0 branch-4.0" >&2
  fi
fi
echo "Base branch: $BASE (branch: $BRANCH)"
[ "$PRINT_ONLY" = "1" ] && exit 0

if git rev-parse --verify --quiet "refs/remotes/$REMOTE/$BASE" >/dev/null; then
  BASE_REF="$REMOTE/$BASE"
else
  BASE_REF="$BASE"
fi
# A backport rebased onto the wrong base replays its whole release branch.
# Same spirit as spark-compile-test-and-retry's "500 changed files means the
# base is wrong" guard. MAX_REPLAY=0 disables.
MAX_REPLAY="${MAX_REPLAY:-40}"
REPLAY="$(git rev-list --count "$BASE_REF..HEAD")"
# Not when the base was typed on the command line: this guard exists to catch bad
# DETECTION, and spark-compile-test-and-retry's 500-changed-files guard is gated
# on `not args.base` for exactly that reason. Telling someone who just passed a
# base to "pass the base explicitly" is not advice.
if [ "$BASE_EXPLICIT" = "0" ] && [ "$MAX_REPLAY" != "0" ] \
   && [ "$REPLAY" -gt "$MAX_REPLAY" ]; then
  echo "$REPLAY commits would replay onto $BASE_REF -- that base is almost" >&2
  echo "certainly wrong (release branch rebased onto master?). Pass the base" >&2
  echo "explicitly, or re-run with MAX_REPLAY=0 if it really is that long." >&2
  exit 1
fi
# The lease we push with is only as good as what the fork held when we
# started, and a failed `git pull --ff-only` still advances the tracking ref
# -- so check containment here, before anything is rewritten.
git fetch --quiet "$FORK_REMOTE" "$BRANCH" 2>/dev/null || true
START_HEAD="$(git rev-parse HEAD)"
# ls-remote, not the tracking ref: `git fetch <remote> <branch>` only writes
# refs/remotes/<remote>/<branch> when the remote's fetch refspec covers it, so a
# branch that really is on the fork can leave the tracking ref absent. That empty
# value then (a) skipped the divergence check below, dropping the "pushed from
# another machine" protection, and (b) made the pinned lease "$BRANCH:", which git
# reads as "must not already exist" -- a guaranteed stale-info rejection, after a
# full clean build.
if FORK_LS="$(git ls-remote --heads "$FORK_REMOTE" "refs/heads/$BRANCH")"; then
  FORK_TIP="$(awk 'NR==1{print $1}' <<<"$FORK_LS")"
else
  echo "cannot reach $FORK_REMOTE to read refs/heads/$BRANCH, so the push lease" >&2
  echo "cannot be pinned safely. Network or auth -- every branch hits it." >&2
  exit 2
fi
if [ -n "$FORK_TIP" ] && ! git merge-base --is-ancestor "$FORK_TIP" "$START_HEAD"; then
  echo "$FORK_REMOTE/$BRANCH is at $FORK_TIP, which this branch does not" >&2
  echo "contain -- pushed from another machine? Reconcile first; a" >&2
  echo "force-push from here would drop those commits." >&2
  exit 1
fi
echo "Rebasing onto $BASE_REF ($REPLAY commit(s) to replay)"

git rebase "$BASE_REF"

# COAUTHOR env var overrides, same as LOCK_HELPER above.
if [ -z "${COAUTHOR:-}" ]; then
  COAUTHOR="$HOME/franktheunicorn/tools/add_coauthor.sh"
  [ -x "$COAUTHOR" ] || COAUTHOR="../franktheunicorn/tools/add_coauthor.sh"
fi
if [ ! -x "$COAUTHOR" ]; then
  echo "add_coauthor.sh not found in $HOME/franktheunicorn/tools or ../franktheunicorn/tools" >&2
  exit 1
fi
# add_coauthor.sh picks its own range -- the first commit it doesn't recognise
# as hers -- which reaches past $BASE_REF whenever the base tip is one of her
# own upstream commits, and filter-repo then rewrites upstream history. Two
# defences: don't call it when every commit already has the trailer (the
# commit-msg hook usually got there first), and undo it if it over-reached.
PRE_COAUTHOR="$(git rev-parse HEAD)"
MISSING_TRAILER=0
for c in $(git rev-list "$BASE_REF..HEAD"); do
  git log -1 --format='%B' "$c" | grep -qi '^[[:space:]]*co-authored-by:.*holden@pigscanfly\.ca' \
    || MISSING_TRAILER=1
done
if [ "$MISSING_TRAILER" = "0" ]; then
  echo "every commit in $BASE_REF..HEAD already has the trailer; skipping $COAUTHOR"
else
  "$COAUTHOR"
  # Re-check: the two disagree on what "already has it" means. This guard wants
  # her address; add_coauthor.sh:20 skips on any `Co-authored-by:` at all, as
  # case-sensitive bytes. So a commit carrying the canonical lowercase trailer
  # for somebody else -- a cherry-pick, a GitHub squash-merge trailer -- sets
  # MISSING_TRAILER=1, then gets skipped, and the only check after it is the
  # ancestry one below, which passes. Branch pushed without her trailer, silently.
  STILL_MISSING=0
  for c in $(git rev-list "$BASE_REF..HEAD"); do
    git log -1 --format='%B' "$c" \
      | grep -qi '^[[:space:]]*co-authored-by:.*holden@pigscanfly\.ca' || STILL_MISSING=1
  done
  if [ "$STILL_MISSING" = "1" ]; then
    echo "$COAUTHOR ran but commits in $BASE_REF..HEAD still lack her trailer:" >&2
    echo "it skips any message that already has some other Co-authored-by line." >&2
    echo "Add hers by hand, then re-run. Not pushing an unattributed branch." >&2
    exit 1
  fi
  if ! git merge-base --is-ancestor "$BASE_REF" HEAD; then
    echo "co-author rewrite reached past $BASE_REF and rewrote upstream" >&2
    echo "commits; resetting to $PRE_COAUTHOR and stopping." >&2
    git reset --hard "$PRE_COAUTHOR"
    echo "add_coauthor.sh takes no arguments -- it picks its own range -- so" >&2
    echo "add the trailer to $BASE_REF..HEAD by hand (or teach it a --refs" >&2
    echo "flag), then re-run." >&2
    exit 1
  fi
fi

# Only --full-test queues for a slot; compile-only stays out of the way.
if [ "$FULL_TEST" = "1" ]; then
  LOCK=(bash "$LOCK_HELPER" --)
else
  LOCK=()
fi

# Into a variable, not straight into a pipe: `git diff | grep -q` reports a
# bad $BASE_REF as "no Scala changes" because grep's 1 hides git's 128.
CHANGED="$(git diff --name-only "$BASE_REF" HEAD)"
if grep -qE '\.(scala|java)$' <<<"$CHANGED"; then
  run_step "${LOCK[@]}" ./dev/lint-scala
else
  echo "no Scala/Java changes vs $BASE_REF; skipping lint-scala"
fi

# clean, always: a rebase rewrites files under zinc and its change detection
# does not survive that, so an incremental pass here greens things it did not
# actually recompile. --full-test needs the jar too, so build `package` and
# hand spark-compile-test-and-retry --skip-build rather than booting sbt twice.
BUILD_TASKS=(clean compile test:compile)
[ "$FULL_TEST" = "1" ] && BUILD_TASKS=(clean package test:compile)
run_step "${LOCK[@]}" ./build/sbt -Phive "${BUILD_TASKS[@]}"

if [ "$FULL_TEST" = "1" ]; then
  # Runs the suites derived from the $BASE_REF diff under the lock, retrying
  # each failure individually. The full suite is spark-presend's job. It's a
  # Python script: invoke directly, not via bash.
  run_step "$SCRIPT_DIR/spark-compile-test-and-retry" --fast --skip-build --base "$BASE_REF"
fi

# Pinned, not bare: worktrees share refs, so another tree's fetch can refresh
# refs/remotes/$FORK_REMOTE/$BRANCH during the build and satisfy a bare lease
# with the very commit we checked against above. Empty = "must not exist".
git push --force-with-lease="$BRANCH:$FORK_TIP" "$FORK_REMOTE" "HEAD:$BRANCH"
echo "Ok all done! (rebased onto $BASE_REF, pushed to $FORK_REMOTE/$BRANCH)"
