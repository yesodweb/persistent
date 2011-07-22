#!/bin/sh

PACKAGES="pool persistent persistent-sqlite persistent-postgresql persistent-mongoDB persistent-template"
CABAL=cabal

for package in $PACKAGES
do
    echo Installing $package
    cd $package
    $CABAL configure --enable-tests && $CABAL build && $CABAL test && ./Setup.lhs install || exit
    cd ..
done
