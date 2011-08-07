# running persistent tests

all tests are ran from the main persistent directory

    cd persistent

Use cabal

    cabal configure --enable-tests

Now run with

    cabal build && dist/build/runtests/runtests

## Different backends

By default the sqlite and postgresql backends are tested.
To test mongoDB use this add a CPP option. You can uncomment this in the cabal file.

    -- cpp-options: -DWITH_MONGODB
