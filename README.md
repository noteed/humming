# Humming - `queue_classic` in Haskell

Humming is a job queue implemented in Haskell on top of PostgreSQL. It is a
port of the Ruby
[`queue_classic`](https://github.com/ryandotsmith/queue_classic) library.

## Example usage

    > QC_DATABASE_URL="postgres://username:password@localhost/database" humming create
    > QC_DATABASE_URL="postgres://username:password@localhost/database" humming drop
    > QC_DATABASE_URL="postgres://username:password@localhost/database" humming enqueue \
        --queue WOUHOU --method play --arguments '{}'
    > QC_DATABASE_URL="postgres://username:password@localhost/database" humming count \
        [--queue WOUHOU]
    > QC_DATABASE_URL="postgres://username:password@localhost/database" humming delete \
        --job 4
    > QC_DATABASE_URL="postgres://username:password@localhost/database" humming lock \
        --queue WOUHOU

## Limitations

- Workers poll for jobs without using PostgreSQL's LISTEN/NOTIFY.
- The table holding the jobs is hard-coded to `queue_classic_jobs`.
