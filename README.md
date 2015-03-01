# Humming - `queue_classic` in Haskell

Humming implements job queues and time-based job scheduling in Haskell on top
of PostgreSQL. The job queue is a port of the Ruby
[`queue_classic`](https://github.com/QueueClassic/queue_classic) library.

This package also provides a `humming` command-line program to interact with
`queue_classic` or Humming.

## Overview

Humming essentially manages two PostgreSQL tables: one for the queues, and one
for the scheduled jobs.

Jobs can be pushed to (or retrieved from) Humming using any programming
language that can talk to PostgreSQL. In particular, you can use this library
in Haskell, [`queue_classic`](https://github.com/QueueClassic/queue_classic) in
Ruby, or [Pueuey](https://github.com/cecton/pueuey) in Python.

Jobs are scheduled by writing to the scheduled jobs table. Moving scheduled
jobs to Humming queues is done by running the `humming schedule` process.

## Example usage

A `humming` executable is provided. Unlike the original `queue_classic`, both
the `humming` executable and Haskell library don't use environment variables.

    > export DB='postgres://username:password@localhost/database'
    > humming create  --database-url $DB
    > humming drop    --database-url $DB
    > humming enqueue --database-url $DB \
        --queue FOO --method play --arguments '{}'
    > humming count   --database-url $DB [--queue FOO]
    > humming delete  --database-url $DB --job 4
    > humming lock    --database-url $DB --queue FOO
    > humming work    --database-url $DB --queue FOO

    > humming schedule --database-url $DB [--queue FOO]

`humming schedule` push endlessly scheduled jobs to their respective queues. It
is possible to take care of only jobs destined to a specific queue with the
`--queue` option. It is safe to run multiple `humming schedule` processes
(possibly on different hosts).

## Limitations

- Uses `procpid` from `pg_stat_activity` which was renamed `pid` in PostgreSQL
  9.2.
- Probably doesn't reconnect after a connection error.
- It should be possible to pass --database-url value via a file.
- The table holding the jobs is hard-coded to `queue_classic_jobs`.
- Not much logging is done (and actually only stdout is used).
- Need to handle signals.
- Doesn't handle more than one queue per worker.
