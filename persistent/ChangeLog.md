## 2.8.3

* add `repsertBy`
* See [#804](https://github.com/yesodweb/persistent/pull/804)

## 2.8.2

* Added support for `sql=` to the unique constraints quasi-quoter so that users can specify the database names of the constraints.

## 2.8.1

* DRY-ed up and exposed several util functions in `Database.Persist.Sql.Util`.
	* Upstream-ed `updatePersistValue`, `mkUpdateText`, and `commaSeparated` from `Database.Persist.MySQL`.
	* De-duplicated `updatePersistValue` from various `Database.Persist.Sql.Orphan.*` modules.
* Batching enhancements to reduce db round-trips.
	* Added `getMany` and `repsertMany` for batched `get` and `repsert`.
	* Added `putMany` with a default/slow implementation. SqlBackend's that support native UPSERT should override this for batching enhancements.
	* Updated `insertEntityMany` to replace slow looped usage with batched execution.
* See [#770](https://github.com/yesodweb/persistent/pull/770)

## 2.8.0

* Switch from `MonadBaseControl` to `MonadUnliftIO`
* Reapplies [#723](https://github.com/yesodweb/persistent/pull/723), which was reverted in version 2.7.3.

## 2.7.3.1

 * Improve error messages when failing to parse database results into Persistent records. [#741](https://github.com/yesodweb/persistent/pull/741)
 * A handful of `fromPersistField` implementations called `error` instead of returning a `Left Text`. All of the implementations were changed to return `Left`. [#741](https://github.com/yesodweb/persistent/pull/741)
 * Improve error message when a SQL insert fails with a custom primary key [#757](https://github.com/yesodweb/persistent/pull/757)

## 2.7.3

* Reverts [#723](https://github.com/yesodweb/persistent/pull/723), which generalized functions using the `BackendCompatible` class. These changes were an accidental breaking change.
* Recommend the `PersistDbSpecific` docs if someone gets an error about converting from `PersistDbSpecific`

## 2.7.2 [DEPRECATED ON HACKAGE]

* Many of the functions have been generalized using the `BackendCompatible` class. [#723](https://github.com/yesodweb/persistent/pull/723)
	* This change was an accidental breaking change and was reverted in 2.7.3.
	* These change will be released in a future version of Persistent with a major version bump.
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
