## Learn more: http://yesodweb.com/book/persistent

A Haskell datastore. Datastores are often referred to as "ORM"s. While 'O' traditionally means object, the concept can be generalized as:

    avoidance of boilerplate serialization

In addition , the ORM concept is a way to make what is usually an un-typed driver type-safe.
In dynamic languages rather than compile time errors, safety comes from creating specific dynamic errors rather than sending non-sense queries to the database.

Persistent's goal is to catch every possible error at compile-time, and it comes close to that.

# Backend agnostic

Supports PostgreSql, Sqlite, MySQL, MongoDB, and Redis.

Persistent is designed to be adaptable to any datastore, and to allow multiple datastores to be used simultaneously.
The serialization layer should be adaptable to any datastore.

Providing a universal query layer will always be limiting.
A major limitation for SQL databases is that the persistent library does not directly provide joins.
However, you can use [Esqueleto](http://hackage.haskell.org/package/esqueleto) with Persistent's serialization to write type-safe SQL queries.
Key-value stores such as Redis can be used with persistent, but only fill out the key-value portion of the API (PersistStore) rather than the query portion (PersistQuery).

Persistent provides several hooks to create backend-specific functionality.
One can always fall back to using the raw database driver or other lower-level or less type-safe libraries and can utilize Persistent for un-serializing the database response to a Haskell record.


## Install from source

Install the Haskell Platform first. Clone this repo and run:

    cabal update
    cabal install cabal-meta cabal-src
    cabal-meta install

# Developing persistent

see development.md
