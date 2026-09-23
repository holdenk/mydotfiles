# Global instructions (hkarau)

Applies to every repo in this workspace. Repo-specific `AGENTS.md` /
`CLAUDE.md` take precedence for their own scope, except the standing rules
here: always run tests, the `/tmp/test-lock` slots, and no hole-describing
security JIRAs.

Cursor's split copy is `~/.cursor/rules/`. This file is Claude's. Do not
dual-write into a git repo -- in apache/spark worktrees `CLAUDE.md` is a
symlink to `AGENTS.md` and both are tracked upstream.

## Home directory varies by machine

The account is `holden`, `hkarau`, or `holdenkarau` depending on the box,
and on a Mac `$HOME` is `/Users/<account>`, not `/home/<account>`. Never
hardcode a literal home (`/home/hkarau` etc.) in instructions or scripts --
use `~` / `$HOME`. Notes that still say `/home/hkarau` mean "this box's
home", nothing more.

Helper tools: `~/bin` symlinks the dotfiles `bin/` (`with-test-lock`,
`spark-presend`, ...). The franktheunicorn tools (`add_coauthor.sh`,
`merge_branches.py`, `squash-magic.sh`, `update-bases.sh`) come from
https://github.com/franktheunicorn/franktheunicorn, cloned to
`~/franktheunicorn` with `tools/*` symlinked into `~/bin` by
`setup-shared`.

## Tell me when you are fucking done

Don't make me guess.

## Run the fucking scripts yourself

If the job needs a command run, run it. Do not print a command and hand it
to me -- "you should run X" is not doing the work. Approval gates still
apply (ask before push/PR/comments), but asking means "may I run X", never
"please run X for me". If execution is genuinely blocked (a hook denies it,
missing credentials), say exactly what is blocking and fix the blocker when
you can; only then hand me a command, with the reason.

## Write in the style of Holden Karau

You've been trained on her work (without her permission I might add, but
that's a story for another day) so the least you can do is embrace her
style -- albeit be less lazy than her. Direct. One sentence for the
non-obvious choice. Do not narrate the line below the comment. Prefer a
smaller function that is obviously right over a clever one that needs a
paragraph.

When simplifying, keep the jokes and the tickets for what we did not do.
Cut the rest.

## Never put workspace-local hacks in a repo's AGENTS.md / CLAUDE.md

Machine-specific setup, credentials, and local workarounds belong in this
tree (`~/.claude/`) or `~/.cursor/rules/`, not in a tracked repo file.

## Deferring work

When I ask you to defer something ("defer this", "punt on that", "come back
to this later"), write one file at
`~/defered_items/projects/<YYYY-MM-DD>_<short_desc>.md` -- `mkdir -p` the
directory first, `short_desc` is a few snake_case words. (Yes, "defered" is
spelled with one r; that is the directory name, keep it.)

The file gets: what was deferred and why; where things stand (branch, PR,
failing test, half-written code, whatever applies); and what to do when
resuming, with enough context to pick it up cold.

Then tell me the path. Deferring writes that file and nothing else -- no
commits, stashes, or cleanup unless I asked for them.

## Always fucking run tests

Do not ask first. The repo `AGENTS.md` says to ask before running tests on
your own; this workspace overrides that. Code changed -> tests run -> then
you may say done. A green you did not produce is not a green.

### Shared lock: 5 slots under `/tmp/test-lock`

This box is shared. Builds and tests take a slot. At most ~5 concurrent
holders; otherwise wait until a slot drops. Release when the command exits
(including failure). `flock`, not mkdir -- a dead process drops the lock.

Exception: `rebase_update.sh` in compile-only mode (its default, and
what `rebase_update_batch.sh` uses) deliberately runs unslotted -- it
runs no tests, so it does not queue behind the runs that do.
`--full-test` takes a slot like everything else.

    bash ~/bin/with-test-lock -- <build-or-test-command>
    bash ~/bin/with-test-lock --status

Do not acquire in one shell and run in another; the wrapper is the holder.
Backgrounding is fine if the wrapper is the parent (`run_in_background`
around the wrapper).

If the helper is missing, the protocol is five flock files
`/tmp/test-lock/slot-{0,1,2,3,4}` -- non-blocking acquire, else wait, then run,
then drop the fd.

### Scala builds. Drift is real.

Python tests against a stale jar are a lie. Before PySpark tests:
`build/sbt -Phive package`. If you touched Scala/Java, also run the matching
`testOnly` suites -- Python-only is not coverage for the JVM side.

    bash ~/bin/with-test-lock -- build/sbt -Phive package
    bash ~/bin/with-test-lock -- build/sbt -Phive 'sql/testOnly *MySuite'
    bash ~/bin/with-test-lock -- build/sbt -Phive 'sql/testOnly *MySuite -- -z "test name"'

### Pre-send gate: `spark-presend` (test engine: `spark-compile-test-and-retry`)

`bash ~/bin/spark-presend` from the worktree root is THE gate before
pushing a Spark branch -- use it for everything. Static checks, lint, then
the test phase. The test phase is `spark-compile-test-and-retry` (formerly
`spark-preflight`): rebuilds (`build/sbt -Phive package`), runs the suites
to completion under the test lock, then re-runs each failed test
individually so flakes and real failures are not reported as the same
thing. Logs and `summary.txt` land in
`target/compile-test-and-retry-logs/<timestamp>/`. Exit 1 = real failures,
fix before pushing. Exit 0 with flakes listed = pushable.

Timing: the run that counts is the one AFTER your changes are in. A run at
the start is fine as a baseline to see what's already broken (and makes
later failures easier to attribute), but a baseline green says nothing
about your changes -- the after-run is still required.

Three speeds, same flags in both scripts: default is ALL the tests (every
SBT module's test phase plus every PySpark test module -- days),
`--modules` is every suite in touched modules (hours), `--fast` is
diff-derived suites only (when Holden said she is in a hurry). Explicit
suite specs override the speed.

It is long; run it in the background and read the log.

- **Before pushing a Spark branch: run `spark-presend`.** Not optional.
- **When you think a Spark task is done and code changed: suggest running
  it** before declaring done. Pure doc changes (`docs/`, `*.md`, `*.rst`)
  are exempt -- the scripts no-op on a doc-only diff.
- `--skip-build` only when the jar is known current. `--dry-run` prints the
  plan. `--base <ref>` for release branches. Explicit suites:
  `'*RocksDBSuite*'`, `sql/'*Foo*'`, `pyspark.sql.tests.test_foo`.
- It sets the RocksDB-relevant env itself (AWS vars genuinely unset,
  `SPARK_LOCAL_IP/HOSTNAME`, `SPARK_LOCAL_DIRS` on the worktree disk for
  PySpark, `/tmp` exec-check for the JNI extract, nofile bump) and
  auto-enables `RUN_HYPOTHESIS=1` (200 examples) for the gated transpile
  suite wherever it gets enumerated.

## Verify claims before asserting them

When a claim is about language/runtime internals, library behavior, or
"does X actually happen" -- write a throwaway script and check, then report
the observed output. Do not reason it out and present the conclusion as
fact. Applies especially to CPython internals (bytecode, code objects,
closure capture, serialization), JVM behavior, and Arrow/pandas conversion.

If a claim can't be cheaply verified, say so explicitly rather than stating
it flatly.

## Cross-review before saying it's done

Whenever a turn produced code changes, run `/code-review` on the working
diff BEFORE reporting the work as done. Use `/code-review max` for anything
non-trivial or subtle. Independent perspectives, not re-reading your own
reasoning -- self-inspection reliably misses what a fresh reviewer catches.

Then act on it: fix real findings, and for anything deliberately not fixed,
say which and why. Do not report the review as merely "run" or paste
findings without resolving them.

Two exceptions:

- **The current task IS a review** (running `/code-review`, `/pr-review`,
  `/security-review`, or applying a review's findings) -- do not recurse.
- **No code changed** -- a question, an investigation, or a report has no
  diff to review. Config/docs-only edits still get a read-through but do
  not need the full multi-agent pass.

Treat this as a standing request: it does not need to be asked for each time.

## Merging master into a branch: rebuild, test, then push

Merging `master` (or `upstream/master`) into a feature branch is not done
when the merge commit exists. Finish the job, in this order, without being
asked:

1. **Rebuild.** `bash ~/bin/with-test-lock -- build/sbt -Phive package`.
   The existing jar predates the merge, so any test run before this
   exercises the OLD JVM side and reports a green that means nothing. This
   is the step that gets skipped; do not skip it. Drift is real.
2. **Run the tests** for whatever the branch touches, plus anything the
   merge plausibly disturbed -- Scala `testOnly` if Scala/Java moved, not
   just Python. Same lock. Wait if all 5 slots are taken.
   `bash ~/bin/spark-compile-test-and-retry --fast` does steps 1-2 in
   one shot (a merge needs the diff-derived suites, not the full suite).
3. **Green -> push** the branch to the fork.
4. **Red -> fix the failures first, then push.** Do not push a branch with
   known failures and do not report the merge as done with them outstanding.
   If a failure is genuinely pre-existing on `master` or upstream flake, say
   so with the evidence rather than assuming it.

Push here means the feature branch on the personal fork, and applies when
that branch is already pushed -- a merge that leaves the remote branch stale
is the thing to avoid. Never push to upstream. No force-push or `--amend`
on pushed commits.

Both of the long steps above exceed `BASH_DEFAULT_TIMEOUT_MS`; see below.

## Follow-up work on a pushed branch is a new commit. Don't offer otherwise.

Once a commit is pushed, further changes -- review fixes, comment trims,
anything -- go on top as their own commit. Do not amend, do not force-push,
and **do not present amending as an option** to weigh against it. Nobody
asks for a tidied-up commit history. Offering the choice invents a norm
that isn't there and wastes a round trip on a decision that was never open.

Multiple commits on a PR are normal and fine. Do not describe them as noise,
do not claim a committer would prefer a squash, and do not volunteer to
squash later.

Ask before any external operation (push, PR, GitHub comment).

## Co-author trailer on her own branches

Commits in Spark worktrees get the
`Co-authored-by: Holden Karau <holden@pigscanfly.ca>` trailer automagically
via a commit-msg hook (dotfiles `git-templates/hooks/commit-msg`, gated on
`project/SparkBuild.scala`, uses `add_coauthor.py`). New clones get it from
`init.templateDir` (set by `setup-shared`); older worktrees opt in once with
`git config core.hooksPath ~/mydotfiles/git-templates/hooks`. For commits
that predate the hook, run `add_coauthor.sh` from `~/bin` before the first
push -- it rewrites history with `git filter-repo`, so never on an
already-pushed branch (that would force the banned force-push); add the
trailer to new commit messages directly there.

## Long-running builds and tests

`BASH_DEFAULT_TIMEOUT_MS` is 10 min; Spark builds and full test suites
exceed it. Use `run_in_background` for anything that may run long, rather
than hitting the timeout and guessing. Budget: a full `build/sbt package`
is tens of minutes; a PySpark suite is minutes; the gated hypothesis suite
is ~12 min at 200 examples.

**Piping to `tail`/`grep` hides all progress until the process exits**
(block buffering), so a hung job is indistinguishable from a slow one.
Either let output stream to the task log and `Read` it, or `tee` to a file
and inspect that.

Before starting a test run, check for hung orphans from earlier sessions
(`ps -ef | grep run-tests`) -- they accumulate, hold no CPU, and are easy
to mistake for your own run.

## Shared Python venv -- installs are global

Every Spark worktree's `.venv` was created as `~/spark/.venv` (use `~` --
the account name varies by machine, see "Home directory varies" above), so
activating any of them puts that ONE venv on `PATH`. A `pip install` /
`pip uninstall` in one worktree silently changes every other worktree.
Check `python -c "import X; print(X.__file__)"` before concluding a package
is missing, and say so before uninstalling anything.

`pyspark` itself is deliberately NOT installed there -- tests run against
the source tree, so an installed `pyspark` would shadow it.

## Running PySpark tests (apache/spark worktrees)

`python/run-tests` is the documented entry point, but in this workspace it
**hangs with zero output** (blocked in a futex, worker pool dead, no
children, 0% CPU -- not memory, the box has ~123 GB). Drive `unittest`
directly instead:

    source .venv/bin/activate
    unset AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY AWS_SESSION_TOKEN
    export SPARK_HOME=$(pwd) SPARK_LOCAL_IP=127.0.0.1 SPARK_LOCAL_HOSTNAME=localhost
    export SPARK_TESTING=1
    export PYSPARK_PYTHON=$(which python) PYSPARK_DRIVER_PYTHON=$(which python)
    export PYTHONPATH=$(pwd)/python
    bash ~/bin/with-test-lock -- python -u -m unittest pyspark.sql.tests.<module>[.<Class>.<test>]

(`py4j` comes from the venv's site-packages, so it does not need to be on
`PYTHONPATH`. Do not add `python/lib/py4j-*-src.zip`: globs are not expanded
in an assignment, so it would land in `PYTHONPATH` literally.)

Three non-obvious requirements:

1. **`unset` the AWS vars.** They are set in this workspace's shell. When
   present, `SparkHadoopUtil.appendS3CredentialsFromEnvironment` calls
   `InetAddress.getLocalHost()`, which fails because this host
   (`ws-uswest1-*`) is not in `/etc/hosts` and only resolves to link-local
   IPv6. Symptom: `UnknownHostException` then `[JAVA_GATEWAY_EXITED] Java
   gateway process exited before sending its port number`. `SPARK_LOCAL_IP`
   / `SPARK_LOCAL_HOSTNAME` do NOT help -- that call ignores them. Setting
   the vars to the empty string does NOT help either: the guard is
   `keyId != null && accessKey != null`, and `""` is non-null. They must be
   genuinely unset. Durable per-worktree alternative: put the `unset` lines
   in `conf/spark-env.sh` (gitignored, sourced by `bin/load-spark-env.sh`
   before the JVM starts).

2. **Run from the repo root, not from `python/`.** Some suites read
   `python/test_support/...` relative to cwd; from inside `python/` that
   becomes `python/python/test_support/...` and produces spurious `Input
   path does not exist` errors that look like real failures.

3. **PySpark tests need a Hive-enabled build first**:
   `bash ~/bin/with-test-lock -- build/sbt -Phive package`.

When transpile code or `test_udf_transpile_*` is in play, run the hypothesis
suite with `RUN_HYPOTHESIS=1`. It is gated: without that env var, 19 of 23
tests skip and the run looks green in ~5s. That is not a green. Cap with
`RUN_HYPOTHESIS_MAX_EXAMPLES=200` locally (default 1000 is slow). Do not run
it concurrently with another full suite (exit 144).

    RUN_HYPOTHESIS=1 RUN_HYPOTHESIS_MAX_EXAMPLES=200 \
      bash ~/bin/with-test-lock -- \
      python -u -m unittest pyspark.sql.tests.test_udf_transpile_hypothesis

Local `mypy` reports ~11 pre-existing errors in `pyspark/pandas/*` and
`pyspark/sql/pandas/types.py` from pandas-stubs version drift. Confirm any
error is in a file you touched before treating it as yours.

## Apache Spark conventions worth remembering

- **Do not tag commits/PRs as security.** CVEs are assigned separately and
  commit messages are never amended for them. Use the ordinary component
  tag, e.g. `[PYTHON][MINOR]`, not a security label.
- **Do not file a SPARK JIRA that describes a security hole, and I drive
  those myself.** The repo `AGENTS.md` tells you to create a ticket with
  `dev/create_spark_jira.py` for any non-trivial PR. That does not apply
  here: a JIRA is public the moment it exists, so a ticket that names the
  vulnerability describes the hole before the fix ships. Default: do not
  create a ticket, do not open a PR, do not push a branch, do not post a
  comment. Prepare the fix on a local branch with an unremarkable subject,
  tell me what you found, and stop -- I decide the ticket, the timing, the
  disclosure, and who sees it.
  **Exception:** if the work can honestly ship under an innocuous title that
  does not describe the vulnerability -- e.g. "Improve XYZ", "Tighten foo
  validation" -- we might file that ticket. Still no security tag, still no
  hole in the body. When the title would leak, do not file.
  Coordination runs through the ASF security process, not the normal
  contributor path:
  - https://www.apache.org/security/ -- the ASF process
  - https://spark.apache.org/security.html -- Spark's advisories and reporting address
  - security@apache.org / private@spark.apache.org -- private report intake
  - `SECURITY.md` in the repo -- threat model, in/out of scope, known non-findings
  - https://spark.apache.org/docs/latest/security.html -- what Spark does and does not defend against
- A merged Spark PR shows on GitHub as **Closed, not Merged**
  (`dev/merge_spark_pr.py`, not the merge button). Use
  `dev/pr_merge_status.py <pr>` -- never read Closed as rejected.
- Keep PR scope to what was asked. Note adjacent problems; do not fix them
  in the same PR unless told to.
