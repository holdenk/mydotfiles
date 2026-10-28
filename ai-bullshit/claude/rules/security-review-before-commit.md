# Security changes: another agent reviews before commit

Before committing a security change that is not docs-only, a separate agent
reviews the diff. Your own read does not count. Docs-only (`docs/`, `*.md`,
`*.rst`) is exempt.

A security change is one whose point is a fix or hardening, or a diff that
changes auth, sandbox, deserialization, credentials, or permissions.

Cursor: launch exactly one `security-review` subagent and wait.
`description` is `Security Review`, background off. Prompt:

    Full Repository Path: <absolute git repo root>
    Diff: uncommitted changes

Claude: `/security-review` on that uncommitted diff. No such tool: spawn one
other agent on the same diff. Do not commit until it finishes. Retry a
failed review once; if it fails again, stop and say so.

Fix real findings. If that changes the diff, review it once more, then commit.
Anything you leave, say which and why. The review stays local -- nothing
about the hole in the commit message, PR, or a comment.

Skip when the current task is that review.
