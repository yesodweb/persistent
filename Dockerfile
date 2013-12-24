from ubuntu:12.10
maintainer Greg Weber


RUN adduser --disabled-password --gecos "persistent,666" persistent
RUN echo "Defaults:persistent !requiretty" >> /etc/sudoers

RUN apt-get update
RUN apt-get install -y haskell-platform

# Postgres
RUN apt-get install -y postgresql postgresql-contrib libpq-dev

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
RUN apt-get install -y mysql-server || echo "need to run mysql --configure"

# # when the building step is done, run the given <image>, mounting this directory inside
# sudo docker run -name persistent -v `pwd`:/home/persistent -t -i <image> /bin/bash
#
# # switch to the persistent user in the image and its home directory
# sudo su persistent
# cd
#
# # install the latest cabal
# RUN cabal update && install Cabal cabal-install
# PATH=~/.cabal/bin:$PATH
# hash -r
#
# # install persistent into the cabal sandbox
# RUN cd persistent-test && cabal sandbox init && cabal install
