# Deferring work

When I ask you to defer something ("defer this", "punt on that", "come back
to this later"), write one file at
`~/defered_items/projects/<YYYY-MM-DD>_<short_desc>.md` -- `mkdir -p` the
directory first, `short_desc` is a few snake_case words. (Yes, "defered" is
spelled with one r; that is the directory name, keep it.)

The file gets:

- what was deferred and why
- where things stand: branch, PR, failing test, half-written code, whatever
  applies
- what to do when resuming, with enough context to pick it up cold

Then tell me the path. Deferring writes that file and nothing else -- no
commits, stashes, or cleanup unless I asked for them.
