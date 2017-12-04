# Running persistent tests using Stack

For testing specific package:

    stack test persistent-sqlite

For appropriate backend specific testing using the package `persistent-test`:

    stack test persistent-test --flag persistent-test:<backend> --exec persistent-test

where <backend> is one liste in the cabal file

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

You can develop just against your preferred backend and the community should help sort out issues with others.

However, we have a Dockerfile that you can use to install all the databases.

    docker build -t persistent .
    docker run -d --name postgres postgres:9.3.5
    docker run --link postgres:postgres --name persistent -v `pwd`:/home/haskell -t -i persistent /bin/bash

This only works on Linux, but you can use Linux on Mac or Windows through Virtualbox.

After building you still need to start up the databases in the background (other than sqlite) that you are testing.
For example:

    mongod --smallfiles &

Docker does not support upstart so just because you install a database does not mean it will be running. You must launch each one in the background.


# Updating Sqlite C Source in the repo

Usually some patches are applied on top of the official sqlite C
source. This section tries to document the patch to make future
updates easier:

* [Workaround for stat64 bug](https://github.com/yesodweb/persistent/commit/0df11d70e936389ca6ed15afb227f6224ad16f22)

* [Workaround for stat64 bug on OSX](https://github.com/yesodweb/persistent/commit/d5836ed5eb76ce6a340442bdfc231895a23c29dc)
