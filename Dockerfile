from stackbrew/ubuntu:14.04
maintainer Greg Weber

RUN apt-get install -y adduser
RUN adduser --disabled-password --gecos "persistent,666" persistent
RUN echo "Defaults:persistent !requiretty" >> /etc/sudoers

RUN apt-get update
RUN apt-get install -y haskell-platform

# Above installs ghc 7.6 to /usr/bin/
# Below install ghc 7.8 to /usr/local/bin/
ADD http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-x86_64-unknown-linux-deb7.tar.xz ghc-7.8.3-x86_64-unknown-linux-deb7.tar.xz
RUN apt-get install -y xz-utils
RUN tar xf ghc-7.8.3-x86_64-unknown-linux-deb7.tar.xz
RUN apt-get install -y g++ make
RUN cd ghc-7.8.3 && ./configure && make install && cd ..
RUN rm -fr ghc-7.8.3-x86_64-unknown-linux-deb7.tar.bz2 ghc-7.8.3


# Postgres
# TODO: getting a failure for this now
# RUN apt-get install -y postgresql postgresql-client postgresql-contrib libpq-dev

# Sqlite
RUN apt-get install -y sqlite3 libsqlite3-dev

# Redis
RUN apt-get install -y redis-server

# MongoDB
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv 7F0CEB10
RUN echo "deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen" >> /etc/apt/sources.list.d/10gen.list
RUN mkdir -p /data/db

RUN apt-get update
RUN apt-get install -y mongodb-10gen || echo "upstart error expected"

# MySQL
RUN apt-get install -y pcre mysql-server || echo "need to run mysql --configure"

USER persistent
ENV HOME /home/persistent

# build the image
#
#     sudo docker build -t persistent .
#
# run the image with the directory mounted
#
#     sudo docker run --name persistent -v `pwd`:/home/persistent -t -i persistent /bin/bash
#
# # switch to the persistent user in the image and its home directory
# su persistent
# cd
#
# # install the latest cabal
# RUN cabal update && cabal install Cabal cabal-install
# PATH=~/.cabal/bin:$PATH
# hash -r
#
# # install persistent into the cabal sandbox
# RUN cd persistent-test && cabal sandbox init && cabal install
#
# # launch databases
# RUN cd persistent-test/db
#
# mongod --dbpath=. --logpath=./mongodb.log --smallfiles --logappend --profile 2 --verbose &
#
# redis-server --save "" # port 6379
#
# mysql_install_db --datadir=/home/persistent/persistent-test/db
# mysqld_safe --datadir=/home/persistent/persistent-test/db

# TODO: change the directory that is used to to be persistent-test/db
# su postgres -c '/usr/lib/postgresql/9.1/bin/postgres -D /var/lib/postgresql/9.1/main --config_file=/etc/postgresql/9.1/main/postgresql.conf' &
# su postgres -c 'createuser -P -d -r -s docker'
# su postgres -c 'createdb -O docker docker'
