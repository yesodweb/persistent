# `persistent-postgresql`

[![Build Status](https://travis-ci.org/yesodweb/persistent-postgresql.svg?branch=master)](https://travis-ci.org/yesodweb/persistent-postgresql) [![Hackage](https://img.shields.io/hackage/v/persistent-postgresql.svg)] ![Hackage-Deps](https://img.shields.io/hackage-deps/v/persistent-postgresql.svg)

A backend for the `persistent` database library for the PostgreSQL database server.

## Development

To run tests on this library, you will need to have a PostgreSQL database server set up and running on your computer.
The tests will expect to connect to a database named `test` using the `postgres` user and no password.
This can be done either via the Postgresql command line or using the `createdb` tool:

```
$ psql -d postgres
postgres=# CREATE DATABASE test;
CREATE DATABASE

-- or,
$ createdb test
```
