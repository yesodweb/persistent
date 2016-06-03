# Building

    stack build

# Running persistent tests

All tests are ran from the persistent-test directory

    cd persistent-test

Use cabal

    cabal configure --enable-tests

If you would like to configure tests with a specific backend that can be enabled
using

    cabal configure -f<backend> --enable-tests

where <backend> is mongodb, postgresql, mysql, etc.

Now run with

    cabal build && dist/build/test/test


## Installing backends

You can develop just against your preferred backend and the community should help sort out issues with others.

However, we have a Dockerfile that you can use to install all the databases.

    docker build -t persistent .
    docker run -d --name postgres postgres:9.3.5
    docker run --link postgres:postgres --name persistent -v `pwd`:/home/haskell -t -i persistent /bin/bash

This only works on Linux, but you can use Linux on Mac or Windows through Virtualbox.

Docker does not support upstart so just because you install a database does not mean it will be running. You must launch each one in the background.
For example:

    mongod --smallfiles &


If someone can contribute information on how to run persistent or MySQL in docker that would be appreciated.
