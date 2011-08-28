#!/bin/bash -x

CABAL=cabal

# install testing dependencies
install_test_deps(){
  $CABAL install HUnit QuickCheck 'hspec >= 0.6.1 && < 0.7' 'file-location >= 0.4 && < 0.5'
}

install_test_deps ||
  $CABAL update && install_test_deps

# also pool and persistent
PACKAGES="persistent-template persistent-sqlite persistent-postgresql persistent-mongoDB"

cabal_install() {
    if [ "$2" == "" ]
    then
      configure_opts="--enable-tests --ghc-options=-Wall --ghc-options=-Werror"
      test="$CABAL test"
    else
      configure_opts=$2
      test=""
    fi
    echo Installing $1
    cd $1
    ($CABAL configure $configure_opts ||
      ($CABAL install --only-dependencies && $CABAL configure $configure_opts)
    ) && $CABAL build && $test && $CABAL check && $CABAL haddock --executables && ./Setup.lhs install || exit 1
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
