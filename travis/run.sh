#!/usr/bin/env bash

set -euxo pipefail

export PATH=/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:$HOME/.cabal/bin:$PATH
ghc --version
cabal --version

if [ "$CABALVER" = "1.18" ]
then
    TEST=--enable-tests
else
    TEST=--run-tests
fi

if [ "$BACKEND" = "none" ]
then
    cabal install --force-reinstalls $TEST $(cat sources.txt)
else
    if [ "$BACKEND" = "postgresql" ]
    then
        psql -c 'create database persistent;' -U postgres
    elif [ "$BACKEND" = "mysql" ]
    then
        mysql -e 'create database persistent;'
    elif [ "$BACKEND" = "zookeeper" ]
    then
        #sudo add-apt-repository -y ppa:yandex-sysmon/zookeeper-3.4
        #sudo apt-get update
        #sudo apt-get install -y libzookeeper-mt-dev zookeeperd
        #sudo mkdir -p /var/log/zookeeper
        #sudo chmod -R 777 /var/log/zookeeper
        #sudo chmod 666 /etc/zookeeper/conf/zoo.cfg
        #echo maxClientCnxns=128 >> /etc/zookeeper/conf/zoo.cfg
        #sudo service zookeeper restart
        #sleep 10
        /usr/share/zookeeper/bin/zkCli.sh create /persistent null
    fi

    cd persistent-test
    cabal install --force-reinstalls --only-dependencies --ghc-options='-j1' --enable-tests -f$BACKEND

    # Make sure we get regular output sent to Travis to avoid it canceling our
    # builds
    cabal configure --enable-tests -f$BACKEND
    cabal build -j1
    cabal test -j1
fi
