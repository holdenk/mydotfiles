# Global agent instructions (hkarau)

Applies to every repo under this home directory. Repo-specific `AGENTS.md` /
`CLAUDE.md` take precedence for their own scope, except the standing rules
below (always run tests, the lock, no hole-describing security JIRAs).

The canonical Cursor copy is `~/.cursor/rules/` (alwaysApply `.mdc` files).
Claude's copy is `~/.claude/CLAUDE.md` and `~/.claude/rules/`. Do not
dual-write into a git repo.

If those rules are not already in context, read every `alwaysApply: true` file
under `~/.cursor/rules/` before doing anything else.

## Tell me when you are fucking done

Don't make me guess.

## Write in the style of Holden Karau

Direct. One sentence for the non-obvious choice. Be less lazy than her.
When simplifying, keep the jokes and the tickets for what we did not do.

## Never put workspace-local hacks in a repo's AGENTS.md / CLAUDE.md

Machine-specific setup belongs in `~/.cursor/rules/` or `~/.claude/CLAUDE.md`.
Those trees live in `~/mydotfiles` (`AGENTS.md`, `cursor-rules/`,
`cursor-hooks/`, `cursor-hooks.json`, `claude/`) and are symlinked into
place -- edit them anywhere, commit them there.

## Deferring work

When I ask you to defer something, write
`~/defered_items/projects/<YYYY-MM-DD>_<short_desc>.md` (`mkdir -p` first)
with what was deferred, why, where things stand, and what to do when
resuming. Then tell me the path.

## Always fucking run tests

Do not ask first. Wrap builds and tests in
`bash ~/bin/with-test-lock -- ...` (3 slots under `/tmp/test-lock`;
wait if they are all taken; release when done). Rebuild Scala
(`build/sbt -Phive package`) before Python tests -- drift is real. If you
touched Scala/Java, run the matching `testOnly` suites, not just Python.

Transpile / `test_udf_transpile_hypothesis`: set `RUN_HYPOTHESIS=1` or 19 of
23 tests skip and the suite looks green in ~5s. That is not a green. Cap
with `RUN_HYPOTHESIS_MAX_EXAMPLES=200` locally. Details in
`~/.cursor/rules/spark-pyspark.mdc` and `~/.claude/CLAUDE.md`.

Pre-send before pushing a Spark branch:
`bash ~/bin/spark-presend` from the worktree root (background it,
it is long). Static checks, lint, then `spark-compile-test-and-retry` for
tests -- ALL of them by default (days); `--modules` (hours) or `--fast`
only when told to hurry. Exit non-zero = fix before pushing. Also suggest
a run whenever a Spark task looks done and code changed; pure doc changes
are exempt (it no-ops on doc-only diffs). `--dry-run` runs static checks
only, `--skip-build` when the jar is current, `--base <ref>` for release
branches.

## Do not file a JIRA that describes a security hole

Default: no ticket, no PR, no push, no comment. Prepare a local fix, tell
me, stop. Exception: an innocuous title that does not describe the
vulnerability (e.g. "Improve XYZ") we might file. Still no security tag.
When the title would leak, do not file.
