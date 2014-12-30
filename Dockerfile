# build the image
#
#     sudo docker build -t persistent .
#
# run the image with the directory mounted
#
#     sudo docker run --name persistent --link mongodb:mongodb -v `pwd`:/home/haskell -t -i persistent /bin/bash
#
# Note the above usage of a link
# For databases besides sqlite, you should find a docker containter and link it
# There is a crane.yml file that automates this
#
# Once in the image you should install persistent dependencies (sandbox is optional)
# RUN cd persistent-test && cabal sandbox init && cabal install --only-dep

FROM zsol/haskell-platform-2014.2.0.0
MAINTAINER Greg Weber

RUN sudo apt-get update

# Sqlite
RUN sudo apt-get install -y sqlite3 libsqlite3-dev

# Postgres
RUN sudo apt-get install -y postgresql-client libpq-dev

# MySQL 
RUN sudo apt-get install -y libpcre3-dev libmysqlclient-dev

ENV PATH .cabal-sandbox/bin:.cabal/bin:$PATH:./
RUN sudo apt-get install -y locales && \
    sudo mkdir -p /var/lib/locales/supported.d && \
    sudo sh -c 'echo "en_US.UTF-8 UTF-8" >> /var/lib/locales/supported.d/local' && \
    sudo dpkg-reconfigure locales && \
    sudo update-locale LANG="en_US.UTF-8 UTF-8"
ENV LANG C.UTF-8
