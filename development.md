# running persistent tests

all tests are ran from the persistent-test directory

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

    sudo docker build .

To run this you need to fill in the brackets on this step:

    sudo docker run -v <absolute-path-to-persisten>:/home/persistent -t -i <image-hash-from-docker-build> /bin/bash

This only works on Linux, but you can use Linux on Mac or Windows through Virtualbox.

After building you still need to start up the databases in the background (other than sqlite) that you are testing.
For example:

    mongod --smallfiles &

Docker does not support upstart since it is designed to run a single process, in this case /bin/bash which you can launch other processes with.
In the future we can setup Angel to run all the databases.
