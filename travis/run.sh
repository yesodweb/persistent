#!/usr/bin/env bash

set -euxo pipefail

psql -c 'create database persistent;' -U postgres
mysql -e 'create database persistent;'
exec stack $ARGS --no-terminal test
