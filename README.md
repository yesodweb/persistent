## Learn more: http://www.yesodweb.com/book/persistent

[![Join the chat at https://gitter.im/yesodweb/persistent](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/yesodweb/persistent?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Build Status](https://travis-ci.org/yesodweb/persistent.svg?branch=master)](https://travis-ci.org/yesodweb/persistent) ![Hackage](https://img.shields.io/hackage/v/persistent.svg) ![Hackage-Deps](https://img.shields.io/hackage-deps/v/persistent.svg)

A Haskell datastore. Datastores are often referred to as "ORM"s. While 'O' traditionally means object, the concept can be generalized as:

    avoidance of boilerplate serialization

In addition , the ORM concept is a way to make what is usually an un-typed driver type-safe.
In dynamic languages rather than compile time errors, safety comes from creating specific dynamic errors rather than sending nonsense queries to the database.

Persistent's goal is to catch every possible error at compile-time, and it comes close to that.

# Backend agnostic

Supports PostgreSql, Sqlite, MongoDB, Redis, ZooKeeper, and many other databases via [persistent-odbc](https://github.com/gbwey/persistent-odbc).
The MySQL backend is in need of a maintainer. Currently there are issues with migrations and support for composite and primary keys is lacking.

Persistent is designed to be adaptable to any datastore, and to allow multiple datastores to be used simultaneously.
The serialization layer should be adaptable to any datastore.

Providing a universal query layer will always be limiting.
A major limitation for SQL databases is that the persistent library does not directly provide joins.
However, you can use [Esqueleto](http://hackage.haskell.org/package/esqueleto) with Persistent's serialization to write type-safe SQL queries.
Key-value stores such as Redis can be used with persistent, but only fill out the key-value portion of the API (PersistStore) rather than the query portion (PersistQuery).

Persistent provides several hooks to create backend-specific functionality.
One can always fall back to using the raw database driver or other lower-level or less type-safe libraries and can utilize Persistent for un-serializing the database response to a Haskell record.


## Install from source

Clone the repo and run `stack build` to build all targets. Persistent
supports many backends. If you have only some of these installed the
[development doc](development.md) shows how to build against a subset of
targets.
