from ubuntu:12.10
maintainer Greg Weber

RUN apt-get update
RUN apt-get install -y haskell-platform

RUN cabal update && install Cabal cabal-install

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
RUN apt-get install -y  mongodb-10gen || echo "upstart error expected"

# MySQL
RUN apt-get install -y mysql-server || echo "need to run mysql --configure"

# RUN cd /home/persistent && cabal sandbox init && cabal install
