#!/bin/bash -e

# allow a CABAL env var to override
CABAL=${CABAL:-cabal}
CONFIGURE_OPTS="--ghc-options=-Wall --ghc-options=-Werror"

# install testing dependencies
$CABAL install HUnit QuickCheck 'hspec >= 0.6.1 && < 0.7' 'file-location >= 0.4 && < 0.5'

# also pool and persistent
PACKAGES="persistent-template persistent-sqlite persistent-postgresql persistent-mongoDB"

cabal_install() {
  echo "Installing $1..."

  (
    cd $1

    if [ "$3" = "--clean" ]; then
      $CABAL clean
    fi

    if ! $CABAL configure $CONFIGURE_OPTS; then
      $CABAL install --only-dependencies
      $CABAL configure $CONFIGURE_OPTS
    fi
    $CABAL build

    if [ "$2" = "test" ]; then
      $CABAL configure --enable-tests
      $CABAL build
      $CABAL test
    fi

    $CABAL check
    $CABAL haddock --executables
    ./Setup.lhs install
    cd ..
  )
}

# required for persistent
cabal_install "pool" "test" $@
# don't run the persistent tests right now
cabal_install "persistent" "" $@

for package in $PACKAGES
do
  cabal_install $package "test"
done

# now run the persistent tests
cabal_install "persistent"
