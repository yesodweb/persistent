#!/usr/bin/env bash

set -euxo pipefail

psql -c 'create database persistent;' -U postgres
mysql -e 'create database persistent;'

case "$BUILD" in
stack)
  exec stack --no-terminal $ARGS test --bench --no-run-benchmarks
  ;;
cabal)
  cabal new-test all
  ;;
esac
set +ex
