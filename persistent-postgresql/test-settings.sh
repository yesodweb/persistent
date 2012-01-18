#!/bin/bash -ex

PGHOST=localhost PGPORT=5432 PGUSER=test PGPASS=test PGDATABASE=test runghc test-settings.hs
