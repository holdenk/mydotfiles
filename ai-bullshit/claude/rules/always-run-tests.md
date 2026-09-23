# Always fucking run tests

Do not ask first. The repo `AGENTS.md` says to ask before running tests on
your own; this workspace overrides that. Code changed -> tests run -> then
you may say done. A green you did not produce is not a green.

## Shared lock: 5 slots under `/tmp/test-lock`

This box is shared. Builds and tests take a slot. At most ~5 concurrent
holders; otherwise wait until a slot drops. Release when the command exits
(including failure). `flock`, not mkdir -- a dead process drops the lock.

    bash ~/bin/with-test-lock -- <build-or-test-command>
    bash ~/bin/with-test-lock --status

Do not acquire in one shell and run in another; the wrapper is the holder.
Backgrounding is fine if the wrapper is the parent (`run_in_background`
around the wrapper).

If the helper is missing, the protocol is five flock files
`/tmp/test-lock/slot-{0,1,2,3,4}` -- non-blocking acquire, else wait, then run,
then drop the fd.

## Scala builds. Drift is real.

Python tests against a stale jar are a lie. Before PySpark tests:
`build/sbt -Phive package`. If you touched Scala/Java, also run the matching
`testOnly` suites -- Python-only is not coverage for the JVM side.

    bash ~/bin/with-test-lock -- build/sbt -Phive package
    bash ~/bin/with-test-lock -- build/sbt -Phive 'sql/testOnly *MySuite'

## Pre-send gate: `spark-presend` (test engine: `spark-compile-test-and-retry`)

`bash ~/bin/spark-presend` from the worktree root is THE gate before
pushing a Spark branch -- use it for everything. Static checks, lint, then
the test phase. The test phase is `spark-compile-test-and-retry` (formerly
`spark-preflight`): rebuilds (`build/sbt -Phive package`), runs the suites
to completion under the test lock, then re-runs each failed test
individually so flakes and real failures are not reported as the same
thing. Logs and `summary.txt` land in
`target/compile-test-and-retry-logs/<timestamp>/`. Exit 1 = real failures,
fix before pushing. Exit 0 with flakes listed = pushable, but read the
flakes.

Timing: the run that counts is the one AFTER your changes are in. A run at
the start is fine as a baseline to see what's already broken (and makes
later failures easier to attribute), but a baseline green says nothing
about your changes -- the after-run is still required.

Three speeds, same flags in both scripts: default is ALL the tests (every
SBT module's test phase plus every PySpark test module -- days),
`--modules` is every suite in touched modules (hours), `--fast` is
diff-derived suites only (when Holden said she is in a hurry). Explicit
suite specs override the speed.

It is long. Run it in the background and read the log; do not sit in a
foreground shell hitting the timeout.

- **Before pushing a Spark branch: run `spark-presend`.** Not optional.
- **When you think a Spark task is done and code changed: suggest running
  it** before declaring done. Pure doc changes (`docs/`, `*.md`, `*.rst`)
  are exempt -- the scripts no-op on a doc-only diff.
- `--skip-build` only when the jar is known current (you just built it).
  `--dry-run` prints the plan. `--base <ref>` for release branches.
  Explicit suites: `'*RocksDBSuite*'`, `sql/'*Foo*'`,
  `pyspark.sql.tests.test_foo`.
- It sets the RocksDB-relevant env itself (AWS vars genuinely unset,
  `SPARK_LOCAL_IP/HOSTNAME`, `SPARK_LOCAL_DIRS` on the worktree disk for
  PySpark, exec-check on `/tmp` for the JNI extract, nofile bump) and
  auto-enables `RUN_HYPOTHESIS=1` (200 examples) for the gated transpile
  suite wherever it gets enumerated.
