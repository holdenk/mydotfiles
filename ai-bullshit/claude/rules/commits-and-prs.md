# Co-author trailers and draft PRs

Two standing rules about what you hand back. Both apply in every repo, not
just Spark.

## Every commit gets both co-author trailers

Every commit message you write ends with both of these:

    Co-authored-by: Holden Karau <holden@pigscanfly.ca>
    Co-authored-by: Holden Karau <holden.karau@snowflake.com>

Write them yourself; do not wait for the hook. The commit-msg hook
(`git-templates/hooks/commit-msg` in the dotfiles) adds exactly one of the two
addresses and only fires in a Spark worktree, so leaning on it silently drops
the snowflake address everywhere and both of them in every other repo. Writing
a trailer the hook then also sees is harmless -- `add_coauthor.py` skips an
address that is already present.

For a branch whose commits predate the hook, `add_coauthor.sh` from `~/bin`
backfills the pigscanfly one. It rewrites history with `git filter-repo`, so
never run it on an already-pushed branch -- that would force the banned
force-push. Add the trailers to new commit messages directly there.

It takes no arguments and ignores the ones you pass, so there is no safe
`--help`: running it anywhere runs `git filter-repo --force` on THAT repo,
rewrites the branch and hard-resets, which silently discards uncommitted work.
Only run it from the worktree whose branch you mean to rewrite.

## Open every PR as a draft

`gh pr create --draft`. Ready-for-review is a state she moves it to
(`gh pr ready`) once CI has had a look; it is not the state you open it in.

Still ask before creating the PR -- draft is the default shape, not a licence
to skip the gate on external operations (push, PR, GitHub comment).

## Then schedule the draft's check-in

Opening the draft is not done-ness. Before ending the turn, arm a
non-blocking check-in ~4 hours out with whatever scheduling the session has
(a scheduled task, a monitored background shell), then re-check hourly until
CI goes green or fails. Spark CI takes hours, so checking sooner is pure
polling. At each check-in: refresh PR state (`gh pr view`, `gh pr checks`),
triage unresolved review comments, fix failing CI, and push fixes as new
commits on the PR branch. Green with nothing outstanding stops the loop; say
so when it stops. If the session has no way to schedule, say so instead of
claiming one.

Pushing fixes to the draft branch and resolving its review threads are
pre-authorized -- that is the loop's purpose. Still hers: `gh pr ready`,
merging, and anything beyond what review and CI pointed at.
