#!/usr/bin/env bash

CABAL=cabal

# also pool and persistent
PACKAGES="persistent-template persistent-sqlite persistent-postgresql persistent-mongoDB"

cabal_install() {
    if [ "$2" == "" ]
    then
      configure_opts="--enable-tests"
      test="$CABAL test"
    else
      configure_opts=$2
      test=""
    fi
    echo Installing $1
    cd $1
    ($CABAL configure $configure_opts ||
      ($CABAL update && $CABAL install HUnit QuickCheck hspec file-location-0.2.3 && $CABAL install --only-dependencies && $CABAL configure $configure_opts)
    ) && $CABAL build && $test && ./Setup.lhs install || exit
    cd ..
}

# required for persistent
cabal_install "pool"
# don't run the persistent tests right now
cabal_install "persistent" " "

for package in $PACKAGES
do
  cabal_install $package
done

# now run the persistent tests
cabal_install "persistent"
