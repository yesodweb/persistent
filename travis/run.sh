#!/usr/bin/env bash

set -euxo pipefail

if [ "$BACKEND" = "none" ]
then
    PACKAGES=$(stack --install-ghc $ARGS query locals | grep '^ *path' | sed 's@^ *path:@@' | grep -v 'persistent-test' )

    PEDANTIC="--pedantic"
    # Turn off pedantic for lts-7, due to the sometimes invalid
    # redundant constraint warnings.
    if [ "$ARGS" = "--resolver lts-7  --stack-yaml stack_lts-10.yaml" ]
    then
        PEDANTIC=""
    fi

    exec stack $ARGS --no-terminal test $PEDANTIC $PACKAGES
else
    if [ "$BACKEND" = "postgresql" ]
    then
        psql -c 'create database persistent;' -U postgres
    elif [ "$BACKEND" = "mysql" ]
    then
        mysql -e 'create database persistent;'
    fi

    cd persistent-test
    exec stack $ARGS --no-terminal test --pedantic --fast persistent-test --flag persistent-test:$BACKEND --exec persistent-test
fi
