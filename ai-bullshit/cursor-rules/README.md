# Cursor global rules (hkarau)

Claude's copy lives in `~/.claude/CLAUDE.md` and `~/.claude/rules/`. These
`.mdc` files are the Cursor dual-write. Claude `@` imports do not resolve
here, so each file is self-contained.

Do not copy any of this into a repo's `AGENTS.md` / `CLAUDE.md`. In apache/spark
worktrees those files are tracked upstream.

| File | When |
|---|---|
| `style.mdc` | always -- voice, done-ness, where hacks belong |
| `defer-items.mdc` | always -- defer requests write `~/defered_items/projects/<date>_<desc>.md` |
| `always-run-tests.mdc` | always -- run tests, `/tmp/test-lock` (5 slots), Scala rebuild |
| `verify-and-review.mdc` | always -- claims, tests, cross-review |
| `git-workflow.mdc` | always -- merge/rebuild/test/push, no amend after push |
| `spark-pyspark.mdc` | Spark worktrees -- venv, Scala + Python how to run tests |
| `spark-conventions.mdc` | Spark worktrees -- security JIRA exception, Closed vs Merged, PR scope |
| `spark-security-jira.mdc` | always -- no hole-describing tickets; innocuous titles maybe |
| `ZZ-index.mdc` | always -- short index + lock helper reminder |

Shared lock helper (same path Claude uses):

    bash ~/bin/with-test-lock -- <build-or-test-command>
    bash ~/bin/with-test-lock --status

`~/AGENTS.md` points here so ancestor-walking agents load the same instructions.
A `sessionStart` hook in `~/.cursor/hooks.json` injects the always-apply rules.
`~/.cursor/cli-config.json` mirrors Claude's deny list and status line.

Two things the rules assume about every machine:

- `$HOME` varies: the account is `holden`, `hkarau`, or `holdenkarau`
  (`/Users/<account>` on Mac). Rules use `~` -- keep it that way.
- `~/bin` also carries the franktheunicorn tools (`add_coauthor.sh` etc.),
  symlinked from `~/franktheunicorn/tools/` by `setup-shared`.
