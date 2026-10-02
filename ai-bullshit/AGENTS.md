# Global agent instructions (hkarau)

Applies to every repo under this home directory. Repo-specific `AGENTS.md` /
`CLAUDE.md` take precedence for their own scope, except the standing rules
below (always run tests, the lock, no hole-describing security JIRAs).

The canonical Cursor copy is `~/.cursor/rules/` (alwaysApply `.mdc` files).
Claude's copy is `~/.claude/CLAUDE.md` and `~/.claude/rules/`. Do not
dual-write into a git repo.

If those rules are not already in context, read every `alwaysApply: true` file
under `~/.cursor/rules/` before doing anything else.

## Home directory varies by machine

The account is `holden`, `hkarau`, or `holdenkarau` depending on the box,
and on a Mac `$HOME` is `/Users/<account>`, not `/home/<account>`. Never
hardcode a literal home (`/home/hkarau` etc.) in instructions or scripts --
use `~` / `$HOME`. Notes that still say `/home/hkarau` mean "this box's
home", nothing more.

## franktheunicorn tools

https://github.com/franktheunicorn/franktheunicorn is cloned to
`~/franktheunicorn` and everything in its `tools/` dir is symlinked into
`~/bin` by `setup-shared` (`add_coauthor.sh`, `merge_branches.py`,
`squash-magic.sh`, `update-bases.sh`, ...).

## Tell me when you are fucking done

Don't make me guess.

## Run the fucking scripts yourself

If the job needs a command run, run it. Do not print a command and hand it
to me. Approval gates still apply (ask before push/PR/comments), but asking
means "may I run X", never "please run X for me". If execution is genuinely
blocked, say exactly what is blocking and fix the blocker when you can;
only then hand me a command, with the reason.

## Write in the style of Holden Karau

Direct. One sentence for the non-obvious choice. Be less lazy than her.
When simplifying, keep the jokes and the tickets for what we did not do.

## Never put workspace-local hacks in a repo's AGENTS.md / CLAUDE.md

Machine-specific setup belongs in `~/.cursor/rules/` or `~/.claude/CLAUDE.md`.
Those trees live in `~/mydotfiles/ai-bullshit/` and are symlinked into
place -- edit them anywhere, commit them there.

## Deferring work

When I ask you to defer something, write
`~/defered_items/projects/<YYYY-MM-DD>_<short_desc>.md` (`mkdir -p` first)
with what was deferred, why, where things stand, and what to do when
resuming. Then tell me the path.

## Check the Java version for the branch

Branches differ on which JDKs they accept, so `$JAVA_HOME` matters. Check what
the branch wants (`java.version` / `java.minimum.version` in `pom.xml`,
`docs/index.md`; master is Java 17/21/25 and rejects Java 25 below 25.0.3),
check what you are running (`java -version` AND `echo $JAVA_HOME` -- they
disagree when nix or sdkman owns `PATH`, and sbt follows `JAVA_HOME`), then set
`JAVA_HOME` to match and say which JDK you used when reporting a result.
`project/SparkBuild.scala`'s `checkJavaVersion` fails the compile with the real
reason, so this is loud, not mysterious.

To see what the host already has: `ls -l /usr/lib/jvm`,
`ls -l /etc/alternatives | grep -i java`, `ls -d ~/.sdkman/candidates/java/*`,
`ls -d /nix/store/*/bin/java`. `setup-shared` defaults `JAVA_HOME` to a Java 21
JDK under `/usr/lib/jvm`, found by feature version so distro naming does not
matter (4.0/4.1 take 17/21 only, 25 is master-only) -- a default, override per
branch. It also exports `SPARK35_JAVA_HOME` (a Java 17); nothing reads it
automatically, use it for a 3.5 backport:
`JAVA_HOME=$SPARK35_JAVA_HOME build/sbt -Phive package`.

For the branch you are actually in, `source ~/bin/set_spark_jdk_to_ok` (or
`eval "$(set_spark_jdk_to_ok)"`). It reads `docs/index.md`, `pom.xml`'s
`java.minimum.version` and SparkBuild's version veto out of the worktree, so it
gets 3.5 right -- 3.5 says Java 8/11/17 and has no `checkJavaVersion`, so
"newest that clears the minimum" would hand it a 25. Prefers 21 then 17, never a
nix JDK, and leaves an already-acceptable JAVA_HOME alone (`-f` re-picks anyway,
for when a 3.5 backport's 17 is still sitting there on master).

Use a **system** JDK from `/usr/lib/jvm`, never a nix one, for RocksDB/LevelDB
suites: a nix JDK's loader never reads `/etc/ld.so.cache`, so the native lib
from rocksdbjni cannot dlopen `libstdc++.so.6` and you get an
`UnsatisfiedLinkError` that looks like a missing package.
`LD_LIBRARY_PATH=/lib64` is not the fix -- it breaks nix binaries
(`GLIBC_2.38 not found`).

## Always fucking run tests

Do not ask first. Wrap builds and tests in
`bash ~/bin/with-test-lock -- ...` (5 slots under `/tmp/test-lock`;
wait if they are all taken; release when done). Rebuild Scala
(`build/sbt -Phive package`) before Python tests -- drift is real. If you
touched Scala/Java, run the matching `testOnly` suites, not just Python.

Transpile / `test_udf_transpile_hypothesis`: set `RUN_HYPOTHESIS=1` or 19 of
23 tests skip and the suite looks green in ~5s. That is not a green. Cap
with `RUN_HYPOTHESIS_MAX_EXAMPLES=200` locally. Details in
`~/.cursor/rules/spark-pyspark.mdc` and `~/.claude/CLAUDE.md`.

Pre-send before pushing a Spark branch:
`~/bin/spark-presend` from the worktree root (background it,
it is long). Static checks, lint, then `spark-compile-test-and-retry` for
tests. Pick the speed yourself -- `--fast` by default, `--modules` for more
complex changes, full (no flag) only when super complex and Holden has not
said she is in a rush. Do not block on asking her; judge, proceed, and say
which speed you chose. On a non-busy box (no other PRs in flight, test slots
free -- check `~/bin/with-test-lock --status` and
`gh pr list --author @me --state open`) it is reasonable to `spark-presend
--fast`, push, open the draft PR, then run a full `spark-presend` in the
background and fix any follow-on failures. On a busy box (multiple PRs in
flight or slots contended), `--fast` or `--modules` alone -- do not pile a
days-long full run on a contended box; note a full run is still owed. Exit
non-zero = fix before pushing. Run it (do not just suggest it) whenever a
Spark task looks done and code changed; pure doc changes are exempt (it
no-ops on doc-only diffs). `--dry-run` runs static checks only,
`--skip-build` when the jar is current, `--base <ref>` for release
branches. Timing: the run that counts is the one AFTER your changes; a
baseline run at the start to see what's already broken is fine but never
substitutes.

## Open every PR as a draft

`gh pr create --draft`, every new PR, every repo. Ready-for-review is a state
she moves it to (`gh pr ready`); it is not the state you open it in. Still ask
before any external operation (push, PR, GitHub comment).

## Then schedule the draft's check-in

Opening the draft is not done-ness. Before ending the turn, arm a
non-blocking check-in ~4 hours out with whatever scheduling the session has
(the loop skill, a scheduled task, a monitored background shell), then
re-check hourly until CI goes green or fails. Spark CI takes hours, so
checking sooner is pure polling. At each check-in: refresh PR state
(`gh pr view`, `gh pr checks`), triage unresolved review comments, fix
failing CI, and push fixes as new commits on the PR branch. Green with
nothing outstanding stops the loop; say so when it stops. If the session has
no way to schedule, say so instead of claiming one.

Pushing fixes to the draft branch and resolving its review threads are
pre-authorized -- that is the loop's purpose. Still hers: `gh pr ready`,
merging, and anything beyond what review and CI pointed at.

## Co-author trailers: both addresses, on every commit

Every commit message you write ends with both of these, in every repo -- not
just Spark:

    Co-authored-by: Holden Karau <holden@pigscanfly.ca>
    Co-authored-by: Holden Karau <holden.karau@snowflake.com>

Write them yourself; do not wait for the hook. It covers exactly one of the two
addresses and only in a Spark worktree, so leaning on it silently drops the
snowflake address everywhere and both of them in every other repo. Writing a
trailer the hook then also sees is harmless -- `add_coauthor.py` skips an
address that is already there.

`add_coauthor.sh` takes no arguments and ignores the ones you pass, so there is
no safe `--help`: running it anywhere runs `git filter-repo --force` on THAT
repo, rewrites the branch and hard-resets, discarding uncommitted work. Only
run it from the worktree whose branch you mean to rewrite.

In Spark worktrees the `holden@pigscanfly.ca` one arrives automagically
via a commit-msg hook (dotfiles `git-templates/hooks/commit-msg`, gated on
`project/SparkBuild.scala`, uses `add_coauthor.py`). New clones get it from
`init.templateDir` (set by `setup-shared`); older worktrees opt in once with
`git config core.hooksPath ~/mydotfiles/git-templates/hooks`. For commits
that predate the hook, run `add_coauthor.sh` from `~/bin` before the first
push -- it rewrites history with `git filter-repo`, so never on an
already-pushed branch (that would force the banned force-push); add the
trailer to new commit messages directly there.

## Do not file a JIRA that describes a security hole

Default: no ticket, no PR, no push, no comment. Prepare a local fix, tell
me, stop. Exception: an innocuous title that does not describe the
vulnerability (e.g. "Improve XYZ") we might file. Still no security tag.
When the title would leak, do not file.
