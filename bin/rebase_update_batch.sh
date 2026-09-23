#!/usr/bin/env bash
# rebase_update_batch.sh -- run rebase_update.sh across a list of branches.
#
# For each branch: switch to it, fast-forward to the fork's latest
# (FORK_REMOTE/<branch>, default origin), then hand off to rebase_update.sh
# (rebase onto the PR base, lint, clean build, force-push back).
#
# A branch that fails is logged and the batch moves on, rolled back to the
# commit it started from so a half-done rebase+rewrite never masquerades as
# the branch state (an aborted rebase does not cover that: the rewrite
# happens after the rebase completes). rebase_update.sh exiting 2 is a
# systematic fault -- bad flag, broken environment -- so the batch stops
# rather than doing it N times. Ends back on the branch you started on
# (best-effort), prints a summary, exit 1 if anything failed.
#
# --full-test / --compile-only pass through to rebase_update.sh. Deliberately
# not --print-base: it exits 0 having pushed nothing, which the summary would
# report as updated.
#
# Worktree-per-branch is the layout here, and a branch checked out elsewhere
# cannot be checked out again -- all requested branches are validated up
# front so the batch fails before burning a build on the early ones.
#
# Usage: rebase_update_batch.sh [--full-test|--compile-only] branch1 [...]

set -u

FORK_REMOTE="${FORK_REMOTE:-origin}"
# Sibling of this script (resolves through the ~/bin symlink), so moving the
# mydotfiles checkout never stales this.
# Split out: a failing readlink nested inside another $() is swallowed, and
# SCRIPT_DIR would silently become the invocation cwd.
SELF="$(readlink -f "${BASH_SOURCE[0]}")" || exit 1
[ -n "$SELF" ] || { echo "cannot resolve own path" >&2; exit 1; }
SCRIPT_DIR="$(cd "$(dirname "$SELF")" && pwd)" || exit 1
REBASE_UPDATE="$SCRIPT_DIR/rebase_update.sh"

PASS_THROUGH=()
while [ $# -ge 1 ]; do
  case "$1" in
    --full-test|--compile-only) PASS_THROUGH+=("$1"); shift ;;
    -*)
      echo "unsupported flag for a batch run: $1" >&2
      echo "usage: $0 [--full-test|--compile-only] branch1 [branch2 ...]" >&2
      exit 2 ;;
    *) break ;;
  esac
done

if [ $# -lt 1 ]; then
  echo "usage: $0 [--full-test|--compile-only] branch1 [branch2 ...]" >&2
  exit 2
fi
for arg in "$@"; do
  case "$arg" in
    -*) echo "flags must come before the branch list (got $arg)" >&2; exit 2 ;;
  esac
done
if [ ! -x "$REBASE_UPDATE" ]; then
  echo "rebase_update.sh not found next to $0 (looked for $REBASE_UPDATE)" >&2
  exit 1
fi

# Via a variable: cd "" is a silent no-op, so `|| exit 1` never fires on a
# failed rev-parse and the batch would run against the wrong tree.
TOPLEVEL="$(git rev-parse --show-toplevel)" || exit 1
cd "$TOPLEVEL" || exit 1
# Every branch would fail at the rebase anyway; fail once, up front, instead.
# --porcelain, not --quiet: untracked files are exactly the dirt that makes
# checkout refuse halfway through, and neither diff form reports them.
if [ -n "$(git status --porcelain)" ]; then
  echo "worktree is dirty; commit or stash before a batch rebase" >&2
  git status --short >&2
  exit 1
fi
# A paused rebase passes both diff checks, and the loop's blind abort below
# would throw it away.
# --git-path, not $TOPLEVEL/.git/...: in a linked worktree .git is a file.
if [ -e "$(git rev-parse --git-path rebase-merge)" ] \
   || [ -e "$(git rev-parse --git-path rebase-apply)" ]; then
  echo "a rebase is in progress here; finish or abort it first" >&2
  exit 1
fi
ORIGINAL_BRANCH="$(git branch --show-current)"
# Without this the restore trap silently no-ops and the batch strands you on
# whichever branch failed last.
if [ -z "$ORIGINAL_BRANCH" ]; then
  echo "detached HEAD; check out a branch before a batch run" >&2
  exit 1
fi

# Validate every branch before touching any of them: a branch held by another
# worktree can never be checked out here, and finding that out on branch 7
# after rebasing 1-6 is the expensive way.
PROBLEMS=()
for branch in "$@"; do
  if ! git rev-parse --verify --quiet "refs/heads/$branch" >/dev/null \
     && ! git rev-parse --verify --quiet "refs/remotes/$FORK_REMOTE/$branch" >/dev/null; then
    PROBLEMS+=("$branch: no such branch locally or on $FORK_REMOTE")
    continue
  fi
  # Skip prunable records: a worktree whose gitdir points at a directory that
  # no longer exists does not actually hold the branch, but it is still listed,
  # so it refused the branch outright. `git worktree prune` would clear these,
  # but do not mutate the repo just to validate it -- ignore them instead.
  held="$(git worktree list --porcelain \
            | awk -v b="refs/heads/$branch" '
                /^worktree /  {wt=substr($0, 10); br=""; prunable=0}  # not $2: paths have spaces
                /^branch /    {br=$2}
                /^prunable/   {prunable=1}
                /^$/          {if (br==b && !prunable) print wt; wt=""; br=""; prunable=0}
                END           {if (br==b && !prunable) print wt}')"
  if [ -n "$held" ] && [ "$held" != "$TOPLEVEL" ]; then
    PROBLEMS+=("$branch: checked out in $held")
  fi
done
if [ ${#PROBLEMS[@]} -gt 0 ]; then
  echo "cannot run this batch here:" >&2
  printf '  %s\n' "${PROBLEMS[@]}" >&2
  echo "(worktree-per-branch? run rebase_update.sh in each tree instead)" >&2
  exit 1
fi

restore_branch() {
  [ -n "${ORIGINAL_BRANCH:-}" ] || return 0
  [ "$(git branch --show-current)" = "$ORIGINAL_BRANCH" ] && return 0
  # An interrupt mid-rebase leaves HEAD detached and switch refuses to run;
  # back out of the rebase first.
  git rebase --abort 2>/dev/null || true
  echo "back to $ORIGINAL_BRANCH"
  git switch --quiet "$ORIGINAL_BRANCH" 2>/dev/null || \
    echo "could not return to $ORIGINAL_BRANCH; check out manually" >&2
}
trap restore_branch EXIT

UPDATED=()
FAILED=()

for branch in "$@"; do
  echo "=== $branch ==="
  # switch, not checkout: `git checkout sql` finds the PATH sql/, quietly
  # checks files out of the index, exits 0 and leaves you on the branch you
  # were already on -- which the next steps would then rebase and force-push
  # under the wrong name.
  if git rev-parse --verify --quiet "refs/heads/$branch" >/dev/null; then
    SWITCH=(git switch "$branch")
  else
    # Fork-only branch. Spelled out rather than left to switch's guessing,
    # which is ambiguous when upstream carries the same name.
    SWITCH=(git switch -c "$branch" --track "$FORK_REMOTE/$branch")
  fi
  if ! "${SWITCH[@]}"; then
    FAILED+=("$branch (switch)")
    continue
  fi
  if [ "$(git branch --show-current)" != "$branch" ]; then
    FAILED+=("$branch (switch did not land on it)")
    continue
  fi
  # --ff-only: a diverged local branch is a human problem, not something to
  # merge-commit over in a batch run. Skipped when the branch is not on the fork
  # yet: there is nothing to fast-forward to, and pulling a nonexistent ref can
  # only fail -- which made a branch's first push through the batch impossible
  # even though rebase_update.sh handles that case fine.
  # Exit status, not just output: ls-remote on an unreachable remote prints
  # nothing and fails, which the old -n test read as "not on the fork yet" -- so
  # a network blip skipped the divergence check and said something untrue.
  if ! FORK_LS="$(git ls-remote --heads "$FORK_REMOTE" "refs/heads/$branch")"; then
    # Systematic, not per-branch: every branch hits an unreachable fork, and
    # rebase_update.sh classifies the same condition as exit 2. Reporting N
    # individual failures would defeat the stop-the-batch contract in the header.
    echo "cannot reach $FORK_REMOTE (network or auth); every branch will hit" >&2
    echo "this, so stopping the batch." >&2
    FAILED+=("$branch (cannot reach $FORK_REMOTE)")
    break
  fi
  if [ -n "$FORK_LS" ]; then
    if ! git pull --ff-only "$FORK_REMOTE" "$branch"; then
      FAILED+=("$branch (pull --ff-only $FORK_REMOTE)")
      continue
    fi
  else
    echo "$branch is not on $FORK_REMOTE yet; skipping the pull"
  fi
  # Roll back to here on failure: past the rebase the work is rewritten, not
  # merely in progress, and `rebase --abort` has nothing left to undo.
  PRE_RUN="$(git rev-parse HEAD)"
  # The +"..." form: expanding an empty array under set -u aborts on bash
  # before 4.4, which is /bin/bash on a Mac.
  if "$REBASE_UPDATE" ${PASS_THROUGH[@]+"${PASS_THROUGH[@]}"}; then
    UPDATED+=("$branch")
  else
    rc=$?
    git rebase --abort 2>/dev/null || true
    if [ "$(git rev-parse HEAD)" != "$PRE_RUN" ]; then
      # Do not swallow a failed switch: reset --hard would then move a DETACHED
      # HEAD while refs/heads/$branch stayed rewritten, and the summary would
      # still claim it rolled back. Say what actually happened instead.
      if git switch --quiet "$branch" 2>/dev/null && git reset --hard "$PRE_RUN"; then
        FAILED+=("$branch (rebase_update rc=$rc, rolled back to $PRE_RUN)")
      else
        FAILED+=("$branch (rebase_update rc=$rc, ROLLBACK FAILED -- still rewritten, expected $PRE_RUN)")
      fi
    else
      FAILED+=("$branch (rebase_update rc=$rc)")
    fi
    # Bad flag or a broken environment fails the same way on every branch.
    if [ "$rc" = "2" ]; then
      echo "rebase_update.sh exited 2 (systematic, not branch-specific);" >&2
      echo "stopping the batch." >&2
      break
    fi
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
