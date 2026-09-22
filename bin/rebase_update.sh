#!/usr/bin/env bash
# rebase_update.sh -- rebase the current Spark branch onto its PR's base
# branch, re-add the co-author trailer, lint (when Scala/Java moved),
# compile, force-push TO THE FORK.
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
# --print-base resolves and prints the base branch, then exits.

set -ex

REMOTE="${REMOTE:-upstream}"
FORK_REMOTE="${FORK_REMOTE:-origin}"
LOCK="${LOCK:-$HOME/my-scripts/with-test-lock}"

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

if [ "${1:-}" = "--print-base" ]; then
  PRINT_ONLY=1; shift
else
  PRINT_ONLY=0
fi

if [ $# -ge 1 ]; then
  BASE="$1"
elif BASE=$(detect_base_from_pr); then
  :
elif BASE=$(detect_base_from_name); then
  :
else
  BASE="master"
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

if git diff --name-only "$BASE_REF" HEAD | grep -qE '\.(scala|java)$'; then
  bash "$LOCK" -- ./dev/lint-scala
else
  echo "no Scala/Java changes vs $BASE_REF; skipping lint-scala"
fi
bash "$LOCK" -- ./build/sbt -Phive compile || bash "$LOCK" -- ./build/sbt -Phive clean compile

git push --force-with-lease "$FORK_REMOTE" "HEAD:$BRANCH"
echo "Ok all done! (rebased onto $BASE_REF, pushed to $FORK_REMOTE/$BRANCH)"
