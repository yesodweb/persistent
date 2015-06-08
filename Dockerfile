# build the image
#
#     docker build -t persistent .
#
# run the image with the directory mounted
#
#     docker run --name persistent --link mongodb:mongodb -v `pwd`:/home/haskell -t -i persistent /bin/bash
#
# Note the above usage of a link
# For databases besides sqlite, you should find a docker containter and link it
# There is a crane.yml file that automates this
#
# Once in the image you should install persistent dependencies (sandbox is optional)
# RUN cd persistent-test && cabal sandbox init && cabal install --only-dep

FROM haskell:7.10
MAINTAINER Greg Weber

RUN apt-get update && \
    # development tools
    apt-get install sudo ca-certificates && \
    # Sqlite
    apt-get install -y sqlite3 libsqlite3-dev && \

    # Postgres
    apt-get install -y postgresql-client libpq-dev && \

    # MySQL
    apt-get install -y libpcre3-dev libmysqlclient-dev && \

    apt-get clean

ENV LC_ALL   C.UTF-8
ENV LANGUAGE C.UTF-8

RUN useradd -m -d /home/haskell -s /bin/bash haskell
RUN mkdir -p /etc/sudoers.d && echo "haskell ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/haskell && chmod 0440 /etc/sudoers.d/haskell
ENV HOME /home/haskell
WORKDIR /home/haskell
USER haskell

ENV PATH .cabal-sandbox/bin:.cabal/bin:$PATH:./
