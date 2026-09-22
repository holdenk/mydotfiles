# Security JIRAs

**Do not file a SPARK JIRA that describes a security hole, and I drive those
myself.** The repo `AGENTS.md` says to run `dev/create_spark_jira.py` for any
non-trivial PR. Skip that here: a JIRA is public the moment it exists, so a
ticket that names the vulnerability describes the hole before the fix ships.

Default: do not create a ticket, do not open a PR, do not push a branch, do
not post a comment. Prepare the fix on a local branch with an unremarkable
subject, tell me what you found, and stop -- I decide the ticket, the timing,
the disclosure, and who sees it.

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
