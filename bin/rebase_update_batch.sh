#!/usr/bin/env bash
# rebase_update_batch.sh -- run rebase_update.sh across a list of branches.
#
# For each branch: check it out, fast-forward to the fork's latest
# (FORK_REMOTE/<branch>, default origin), then hand off to rebase_update.sh
# (rebase onto the PR base, compile + test:compile, force-push back).
#
# A branch that fails (checkout, pull, or rebase_update) is logged and the
# batch moves on; a mid-rebase failure is aborted so later branches can
# still check out. Ends back on the branch you started on (best-effort),
# prints a summary, exit 1 if anything failed.
#
# Usage: rebase_update_batch.sh branch1 [branch2 ...]

set -u

FORK_REMOTE="${FORK_REMOTE:-origin}"
# Sibling of this script (resolves through the ~/bin symlink), so moving the
# mydotfiles checkout never stales this.
SCRIPT_DIR="$(cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" && pwd)"
REBASE_UPDATE="$SCRIPT_DIR/rebase_update.sh"

if [ $# -lt 1 ]; then
  echo "usage: $0 branch1 [branch2 ...]" >&2
  exit 2
fi
if [ ! -x "$REBASE_UPDATE" ]; then
  echo "rebase_update.sh not found next to $0 (looked for $REBASE_UPDATE)" >&2
  exit 1
fi

cd "$(git rev-parse --show-toplevel)" || exit 1
ORIGINAL_BRANCH="$(git branch --show-current)"

restore_branch() {
  [ -n "${ORIGINAL_BRANCH:-}" ] || return 0
  [ "$(git branch --show-current)" = "$ORIGINAL_BRANCH" ] && return 0
  # An interrupt mid-rebase leaves HEAD detached and checkout refuses to
  # run; back out of the rebase first.
  git rebase --abort 2>/dev/null || true
  echo "back to $ORIGINAL_BRANCH"
  git checkout --quiet "$ORIGINAL_BRANCH" 2>/dev/null || \
    echo "could not return to $ORIGINAL_BRANCH; check out manually" >&2
}
trap restore_branch EXIT

UPDATED=()
FAILED=()

for branch in "$@"; do
  echo "=== $branch ==="
  if ! git checkout "$branch"; then
    FAILED+=("$branch (checkout)")
    continue
  fi
  # --ff-only: a diverged local branch is a human problem, not something to
  # merge-commit over in a batch run.
  if ! git pull --ff-only "$FORK_REMOTE" "$branch"; then
    FAILED+=("$branch (pull --ff-only $FORK_REMOTE)")
    continue
  fi
  if "$REBASE_UPDATE"; then
    UPDATED+=("$branch")
  else
    git rebase --abort 2>/dev/null || true
    FAILED+=("$branch (rebase_update)")
  fi
done

echo
echo "=== batch summary ==="
if [ ${#UPDATED[@]} -gt 0 ]; then
  printf 'updated: %s\n' "${UPDATED[@]}"
else
  echo "updated: none"
fi
if [ ${#FAILED[@]} -gt 0 ]; then
  printf 'FAILED:  %s\n' "${FAILED[@]}"
  exit 1
fi
echo "all branches updated."
