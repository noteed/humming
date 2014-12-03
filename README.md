# Humming - `queue_classic` in Haskell

Humming is a job queue implemented in Haskell on top of PostgreSQL. It is a
port of the Ruby
[`queue_classic`](https://github.com/QueueClassic/queue_classic) library.

**This is the development branch to bring Humming to `queue_classic` 3.0.**

## Example usage

A `humming` executable is provided. Unlike the original `queue_classic`, both
the `humming` executable and Haskell library don't use environment variables.

    > humming create  --database-url postgres://username:password@localhost/database
    > humming drop    --database-url postgres://username:password@localhost/database
    > humming enqueue --database-url postgres://username:password@localhost/database \
        --queue FOO --method play --arguments '{}'
    > humming count   --database-url postgres://username:password@localhost/database \
        [--queue FOO]
    > humming delete  --database-url postgres://username:password@localhost/database \
        --job 4
    > humming lock    --database-url postgres://username:password@localhost/database \
        --queue FOO
    > humming work    --database-url postgres://username:password@localhost/database \
        --queue FOO

## Limitations

- Probably doesn't reconnect after a connection error.

- It should be possible to pass --database-url value via a file.
- Workers poll for jobs without using PostgreSQL's LISTEN/NOTIFY.
- The table holding the jobs is hard-coded to `queue_classic_jobs`.
- Not much logging is done (and actually only stdout is used).

