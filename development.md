# Running persistent tests using Stack

For testing specific package:

    stack test persistent-sqlite

For appropriate backend specific testing using the package `persistent-test`:

    stack test persistent-test --flag persistent-test:<backend> --exec persistent-test

where <backend> is one listed in the cabal file

# Running persistent tests using Cabal

All tests are ran from the persistent-test directory

    cd persistent-test

Use cabal

    cabal configure --enable-tests

If you would like to configure tests with a specific backend that can be enabled
using

    cabal configure -f<backend> --enable-tests

where <backend> is one of mongodb/postgresql/mysql/couchdb.

Now run with

    cabal build && dist/build/test/test


# Backends

By default the sqlite backend is tested.
To test other backends, you can give a flag described in persisten-test.

    cabal configure -fmongodb --enable-tests


## Installing backends

For an easy install we recommend running a database from a docker container.
Lets develop easy entry points for testing a database using a command runner.

    just --list
    just test-mongo

[Installing just is easy](https://github.com/casey/just/releases).
Or you can look in `./bin/` for the commands it will call and run them directly
Or
Lets develop a docker-compose file for running different databases.


# Updating Sqlite C Source in the repo

Usually some patches are applied on top of the official sqlite C
source. This section tries to document the patch to make future
updates easier:

* [Workaround for stat64 bug](https://github.com/yesodweb/persistent/commit/0df11d70e936389ca6ed15afb227f6224ad16f22)

* [Workaround for stat64 bug on OSX](https://github.com/yesodweb/persistent/commit/d5836ed5eb76ce6a340442bdfc231895a23c29dc)
