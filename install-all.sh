#!/bin/sh

PACKAGES="pool persistent persistent-template persistent-sqlite persistent-postgresql persistent-mongoDB"
CABAL=cabal

for package in $PACKAGES
do
    echo Installing $package
    cd $package
    ($CABAL configure --enable-tests ||
      ($CABAL install --only-dependencies --enable-tests && $CABAL configure --enable-tests)
    ) && $CABAL build && $CABAL test && ./Setup.lhs install || exit
    cd ..
done
