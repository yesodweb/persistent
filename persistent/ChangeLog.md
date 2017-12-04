## 2.7.2.1

* Recommend the `PersistDbSpecific` docs if someone gets an error about converting from `PersistDbSpecific`

## 2.7.2

* Many of the functions have been generalized using the `BackendCompatible` class. [#723](https://github.com/yesodweb/persistent/pull/723)
* Add raw sql quasi quoters [#717](https://github.com/yesodweb/persistent/pull/717)

## 2.7.1

* Added an `insertUniqueEntity` function [#718](https://github.com/yesodweb/persistent/pull/718)
* Added `BackendCompatible` class [#701](https://github.com/yesodweb/persistent/pull/701)

## 2.7.0

* Fix upsert behavior [#613](https://github.com/yesodweb/persistent/issues/613)
* Atomic upsert query fixed for arithmatic operations [#662](https://github.com/yesodweb/persistent/issues/662)
* Haddock and test coverage improved for upsert

## 2.6.1

* Fix edge case for `\<-. [Nothing]`
* Introduce `connMaxParams`
* Add 'getJustEntity' and 'insertRecord' convenience function
* Minor Haddock improvment

## 2.6

* Add `connUpsertSql` type for providing backend-specific upsert sql support.

## 2.5

* read/write typeclass split
* add insertOrGet convenience function to PersistUnique

## 2.2.4.1

* Documentation updates [#515](https://github.com/yesodweb/persistent/pull/515)

## 2.2.4

* Workaround for side-exiting transformers in `runSqlConn` [#516](https://github.com/yesodweb/persistent/issues/516)

## 2.2.3

* PersistField instance for Natural
* better oracle support in odbc

## 2.2.2

* Add liftSqlPersistMPool function
* support http-api-data for url serialization

## 2.2.1

* Migration failure message with context
* Fix insertKey for composite keys

## 2.2

* Add a `RawSql` instance for `Key`. This allows selecting primary keys using functions like `rawSql`. [#407](https://github.com/yesodweb/persistent/pull/407)
* SqlBackend support for an optimized `insertMany`

## 2.1.6

Important! If persistent-template is not upgraded to 2.1.3.3
you might need to make sure `Int64` is in scope for your model declarations.

* add showMigration function
* explicitly use Int64 for foreign key references

## 2.1.5

Add `dbIdColumnsEsc` to Sql.Utils.
Used in persistent-postgresql 2.1.5.2

## 2.1.4

* Fix getBy with a primary key. #342

## 2.1.3

* Break self-referencing cycles in the entity declarations

## 2.1.2

* Error with `Double`s without a decimal part [#378](https://github.com/yesodweb/persistent/issues/378)
* `runSqlPool` does not perform timeout checks.

## 2.1.1.6

* One extra feature for #939: use `logDebugN` instead

## 2.1.1.5

* Better SQL logging [Yesod issue #939](https://github.com/yesodweb/yesod/issues/939)

## 2.1.1.3

Parse UTCTime in 8601 format [#339](https://github.com/yesodweb/persistent/issues/339)

## 2.1.1.1

Support for monad-control 1.0
