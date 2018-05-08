# Building with Backends

With all required backends installed, `stack build` can build all packages
listed in `stack.yaml` and is equivalent to;

```
> stack build persistent persistent-template persistent-sqlite persistent-test
persistent-mongoDB persistent-mysql persistent-postgresql persistent-redis
```

To build when missing backends, such as mysql and postgres shown here, drop the
targets not installed:

```
> stack build persistent-mysql
...
    Process exited with code: ExitFailure 1
    Configuring mysql-0.1.4...
    setup: The program 'mysql_config' is required but it could not be found
    
> stack build persistent-postgresql
...
    Process exited with code: ExitFailure 1
    Configuring postgresql-libpq-0.9.4.0...
    setup: The program 'pg_config' is required but it could not be found.
    
> stack build persistent persistent-template persistent-sqlite persistent-test
persistent-mongoDB persistent-redis
...
Completed 6 action(s).
```

# Running persistent tests using Stack

For testing specific package:

    > stack test persistent-sqlite

For appropriate backend specific testing using the package `persistent-test`:

    > stack test persistent-test --flag persistent-test:<backend> --exec persistent-test

where <backend> is one listed in the cabal file

# Running persistent tests using Cabal

All tests are ran from the persistent-test directory

    > cd persistent-test

Use cabal

    > cabal configure --enable-tests

If you would like to configure tests with a specific backend that can be enabled
using

    > cabal configure -f<backend> --enable-tests

where <backend> is one of mongodb/postgresql/mysql/couchdb.

Now run with

    > cabal build && dist/build/test/test

# Testing Backends

By default the sqlite backend is tested.
To test other backends, you can give a flag described in persisten-test.

    > cabal configure -fmongodb --enable-tests

## Installing backends

For an easy install we recommend running a database from a docker container.
Lets develop easy entry points for testing a database using a command runner.

    > just --list
    > just test-mongo

[Installing just is easy](https://github.com/casey/just/releases).
Or you can look in `./bin/` for the commands it will call and run them directly
Or
Lets develop a docker-compose file for running different databases.
