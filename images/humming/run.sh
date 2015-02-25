#! /bin/bash

DATA=/var/lib/postgresql/9.1/main

sudo -u postgres \
  /usr/lib/postgresql/9.1/bin/postgres \
    -c data_directory=$DATA \
    -c config_file=/etc/postgresql/9.1/main/postgresql.conf &

sleep 5 # TODO Use pgready

humming --help

DB="dbname=docker user=docker password=docker host=127.0.0.1"
humming create --database-url "$DB"
humming unlock-deads --database-url "$DB"
humming enqueue --database-url "$DB" --queue FOO --method play --arguments '{}'
humming enqueue --database-url "$DB" --queue FOO --method play --arguments '{}'
humming count --database-url "$DB"
humming work --database-url "$DB" --queue FOO --once
humming count --database-url "$DB"
humming drop --database-url "$DB"

# TODO Don't use pseudo-Docker port.
DB_PORT_5432_TCP_ADDR=127.0.0.1 Tests
