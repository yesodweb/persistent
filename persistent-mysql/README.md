# `persistent-mysql`

[![Build Status](https://travis-ci.org/yesodweb/persistent-mysql.svg?branch=master)](https://travis-ci.org/yesodweb/persistent-mysql) [![Hackage](https://img.shields.io/hackage/v/persistent-mysql.svg)] ![Hackage-Deps](https://img.shields.io/hackage-deps/v/persistent-mysql.svg)

A backend for the `persistent` database library for the MySQL database server.

## Development

To run tests on this library, you will need to have a MySQL database server set up and running on your computer.
The test suite expects to see databases named `test` and `foo`, and a user with username `test` and password `test`. You can set this up with roughly as follows:

```
mysql -u root # MySQL root username and password may vary
CREATE DATABASE test;
CREATE DATABASE foo;
CREATE USER 'test'@'localhost' IDENTIFIED BY 'test';
GRANT ALL on test.* TO 'test'@'localhost';
GRANT ALL on foo.* TO 'test'@'localhost';
USE test;
```
