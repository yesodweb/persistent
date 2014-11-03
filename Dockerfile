FROM zsol/haskell-platform-2014.2.0.0
MAINTAINER Greg Weber

RUN sudo apt-get update

# Postgres
RUN sudo apt-get install -y postgresql postgresql-client postgresql-contrib libpq-dev

# Sqlite
RUN sudo apt-get install -y sqlite3 libsqlite3-dev

# Redis
RUN sudo apt-get install -y redis-server

# MongoDB
RUN sudo apt-key adv --keyserver keyserver.ubuntu.com --recv 7F0CEB10 && \
    sudo sh -c 'echo "deb http://downloads-distro.mongodb.org/repo/ubuntu-upstart dist 10gen" >> /etc/apt/sources.list.d/10gen.list' && \
    sudo mkdir -p /data/db && \
    sudo apt-get update && \
    sudo apt-get install -y mongodb-10gen

# MySQL
RUN apt-get install -y libpcre3-dev mysql-server libmysqlclient-dev || echo "need to run mysql --configure"

ENV PATH .cabal-sandbox/bin:.cabal/bin:$PATH:./
RUN sudo apt-get install -y locales && \
    sudo mkdir -p /var/lib/locales/supported.d && \
    sudo sh -c 'echo "en_US.UTF-8 UTF-8" >> /var/lib/locales/supported.d/local' && \
    sudo dpkg-reconfigure locales && \
    sudo update-locale LANG="en_US.UTF-8 UTF-8"
ENV LANG C.UTF-8

# build the image
#
#     sudo docker build -t persistent .
#
# run the image with the directory mounted
#
#     sudo docker run --name persistent -v `pwd`:/home/haskell -t -i persistent /bin/bash
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

# I would like to run postgres as the persistent user
# but I am not sure how. The below is for root
# /usr/lib/postgresql/9.3/bin/postgres --config_file=/etc/postgresql/9.3/main/postgresql.conf
# su postgres -c 'createuser -P -d -r -s persistent'
# su postgres -c 'createdb -O persistent persistent'
