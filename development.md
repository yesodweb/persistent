# running persistent tests

all tests are ran from the persistent-test directory

    cd persistent-test

Use cabal

    cabal configure --enable-tests

Now run with

    cabal build && dist/build/test/test

## Different backends

By default the sqlite backend is tested.
To test other backends, use the CPP options. To test mongoDB use add this CPP option or uncomment this in the cabal file:

    -- cpp-options: -DWITH_MONGODB
