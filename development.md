# Running persistent tests using Stack

For testing specific package:

    stack test persistent-sqlite

For appropriate backend specific flag:

    stack test persistent-test --flag persistent-test:<backend>

where <backend> is one of mongodb/postgresql/mysql/couchdb.

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

If someone can contribute information on how to run persistent or MySQL that would be appreciated.
