#!/usr/bin/env bash
# rebase_update.sh -- rebase the current Spark branch onto its PR's base
# branch, re-add the co-author trailer, build-check, force-push TO THE FORK.
#
# Build-check modes:
#   --compile-only (default)  compile + test:compile, no with-test-lock
#                             slot needed.
#   --full-test               lint-scala when Scala/Java moved, then
#                             spark-compile-test-and-retry --fast --base
#                             <base> (takes a lock slot, rebuilds, runs the
#                             suites derived from the base diff with
#                             per-test retries).
# spark-presend stays the real lint/test gate; the default is the quick
# check that the rebase didn't break the build, test sources included.
#
# Base branch resolution, first hit wins:
#   1. explicit argument:        rebase_update.sh branch-3.5
#   2. the open PR for this branch on apache/spark (via gh)
#   3. a version embedded in the branch name that matches an upstream
#      branch (foo-branch-3.5, bar-4.x, baz-4.0, ...)
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

REMOTE="${REMOTE:-upstream}"
FORK_REMOTE="${FORK_REMOTE:-origin}"

cd "$(git rev-parse --show-toplevel)"
BRANCH="$(git branch --show-current)"
if [ -z "$BRANCH" ]; then
  echo "detached HEAD; check out a branch first" >&2
  exit 1
fi

# Best-effort freshness for detection and the rebase target; offline is fine.
git fetch --quiet "$REMOTE" 2>/dev/null || true

detect_base_from_pr() {
  command -v gh >/dev/null 2>&1 || return 1
  # No --author @me: it switches --head to prefix matching and a same-named
  # backport branch (foo-4.x-r2) wins over the exact match. Filter exactly.
  gh pr list --repo apache/spark --head "$BRANCH" --state open \
      --json baseRefName,headRefName \
      --jq '.[] | select(.headRefName=="'"$BRANCH"'") | .baseRefName' \
      2>/dev/null | head -n 1 | grep -v '^$' || return 1
}

detect_base_from_name() {
  # Both an explicit "branch-N.M" and any bare N.M / N.x in the name are
  # candidates; first one that exists as an upstream branch wins (so
  # ivy2.5.3 doesn't resolve to 2.5, and foo-branch-9.9-4.0 finds 4.0).
  local candidates c
  candidates=$( { grep -oE 'branch-[0-9]+\.[0-9x]+' <<<"$BRANCH" || true;
                  grep -oE '[0-9]+\.[0-9x]+' <<<"$BRANCH" \
                    | sed 's/^/branch-/' || true; } )
  for c in $candidates; do
    if git rev-parse --verify --quiet "refs/remotes/$REMOTE/$c" >/dev/null; then
      echo "$c"
      return 0
    fi
  done
  return 1
}

PRINT_ONLY=0
FULL_TEST=0
BASE=""
while [ $# -ge 1 ]; do
  case "$1" in
    --print-base)   PRINT_ONLY=1 ;;
    --full-test)    FULL_TEST=1 ;;
    --compile-only) FULL_TEST=0 ;;
    --*)
      echo "unknown flag: $1 (want --compile-only, --full-test, or --print-base)" >&2
      exit 2 ;;
    *)
      if [ -n "$BASE" ]; then
        echo "unexpected extra argument: $1" >&2
        exit 2
      fi
      BASE="$1" ;;
  esac
  shift
done

if [ -z "$BASE" ]; then
  if BASE=$(detect_base_from_pr); then
    :
  elif BASE=$(detect_base_from_name); then
    :
  else
    BASE="master"
  fi
fi
echo "Base branch: $BASE (branch: $BRANCH)"
[ "$PRINT_ONLY" = "1" ] && exit 0

if git rev-parse --verify --quiet "refs/remotes/$REMOTE/$BASE" >/dev/null; then
  BASE_REF="$REMOTE/$BASE"
else
  BASE_REF="$BASE"
fi
echo "Rebasing onto $BASE_REF"

git rebase "$BASE_REF"

COAUTHOR="$HOME/franktheunicorn/tools/add_coauthor.sh"
[ -x "$COAUTHOR" ] || COAUTHOR="../franktheunicorn/tools/add_coauthor.sh"
if [ ! -x "$COAUTHOR" ]; then
  echo "add_coauthor.sh not found in $HOME/franktheunicorn/tools or ../franktheunicorn/tools" >&2
  exit 1
fi
"$COAUTHOR"

if [ "$FULL_TEST" = "1" ]; then
  # Siblings of this script (resolve through the ~/bin symlink), so moving
  # the mydotfiles checkout never stales these. LOCK_HELPER env var overrides.
  SCRIPT_DIR="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" && pwd)"
  LOCK_HELPER="${LOCK_HELPER:-$SCRIPT_DIR/with-test-lock}"
  if git diff --name-only "$BASE_REF" HEAD | grep -qE '\.(scala|java)$'; then
    bash "$LOCK_HELPER" -- ./dev/lint-scala
  else
    echo "no Scala/Java changes vs $BASE_REF; skipping lint-scala"
  fi
  # Rebuilds and runs the suites derived from the $BASE_REF diff under the
  # lock, retrying each failure individually. The full suite is
  # spark-presend's job. It's a Python script: invoke directly, not via bash.
  "$SCRIPT_DIR/spark-compile-test-and-retry" --fast --base "$BASE_REF"
else
  ./build/sbt -Phive compile test:compile || ./build/sbt -Phive clean compile test:compile
fi

git push --force-with-lease "$FORK_REMOTE" "HEAD:$BRANCH"
echo "Ok all done! (rebased onto $BASE_REF, pushed to $FORK_REMOTE/$BRANCH)"
