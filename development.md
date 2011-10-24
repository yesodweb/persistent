# running persistent tests

all tests are ran from the persistent-test directory

    cd persistent-test

Use cabal

    cabal configure --enable-tests

Now run with

    cabal build && dist/build/test/test

## Different backends

By default the sqlite and postgresql backends are tested.
To test mongoDB use this add a CPP option. You can uncomment this in the cabal file.

    -- cpp-options: -DWITH_MONGODB
