#! /bin/bash

DATA=/var/lib/postgresql/9.1/main

sudo -u postgres \
  /usr/lib/postgresql/9.1/bin/postgres \
    -c data_directory=$DATA \
    -c config_file=/etc/postgresql/9.1/main/postgresql.conf &

sleep 5 # TODO Use pgready

humming --help

humming create --database-url "dbname=docker user=docker password=docker host=127.0.0.1"
