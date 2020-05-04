# Changelog for persistent-postgresql

## (Unreleased) 2.11.0.0

* Implement interval support. [#1053](https://github.com/yesodweb/persistent/pull/1053)

* [#1060](https://github.com/yesodweb/persistent/pull/1060)
  * The QuasiQuoter now supports `OnDelete` and `OnUpdate` cascade options.
* Handle foreign key constraint names over 63 characters. See [#996](https://github.com/yesodweb/persistent/pull/996) for details.
* [#1072](https://github.com/yesodweb/persistent/pull/1072) Refactored `test/JSONTest.hs` to use `hspec`
  * added `runConn_` to run a db connection and return result
  * Renamed `db` to `runConnAssert` in `test/PgInit.hs` for clarity
  * Ran `test/ArrayAggTest.hs` (which was previously written but not being run)

## 2.10.1.2

* Fix issue with multiple foreign keys on single column. [#1010](https://github.com/yesodweb/persistent/pull/1010)

## 2.10.1.1

* Compatibility with latest persistent-template for test suite [#1002](https://github.com/yesodweb/persistent/pull/1002/files)

## 2.10.1

* Added support for the `constraint=` attribute to the Postgresql backend. [#979](https://github.com/yesodweb/persistent/pull/979)

## 2.10.0

* Added question mark operators (`(?.), (?|.), (?&.)`) to `Database.Persist.Postgresql.JSON` [#863](https://github.com/yesodweb/persistent/pull/863)
* Changes to certain types:
    * `PersistValue`: added `PersistArray` data constructor
    * `Filter`: Changed the `filterValue :: Either a [a]` to `filterValue :: FilterValue`

## 2.9.1
* Add `openSimpleConnWithVersion` function. [#883](https://github.com/yesodweb/persistent/pull/883)

## 2.9.0

* Added support for SQL isolation levels to via SqlBackend. [#812]
* Fix [832](https://github.com/yesodweb/persistent/issues/832): `repsertMany` now matches `mapM_ (uncurry repsert)` and is atomic.

## 2.8.2

Added module `Database.Persist.Postgresql.JSON` [#793](https://github.com/yesodweb/persistent/pull/793)

* `PersistField` and `PersistFieldSql` instances for `Data.Aeson.Value`
* Filter operators `(@>.)` and `(<@.)` to filter on JSON values

## 2.8.1.1

* Added a more detailed error message when a `numeric` column's scale and precision can't be parsed. [#781](https://github.com/yesodweb/persistent/pull/781)

## 2.8.1

* Implemented `connPutManySql` to utilize batched `putMany`. [#770](https://github.com/yesodweb/persistent/pull/770)

## 2.8.0

* Switch from `MonadBaseControl` to `MonadUnliftIO`

## 2.6.3

* Added new function `migrateEnableExtension`, to enable Postgres extensions in migrations.

## 2.6.2.2

* Because `text` and `varchar` are synonyms in Postgresql, don't attempt to migrate between them. [#762](https://github.com/yesodweb/persistent/pull/762)

## 2.6.2.1

* Fix bug where, if a custom column width was set, the field would be migrated every time [#742](https://github.com/yesodweb/persistent/pull/742)

## 2.6.2

* Expose new functions: `withPostgresqlPoolWithVersion`, `withPostgresqlConnWithVersion` and `createPostgresqlPoolModifiedWithVersion`.

## 2.6.1

* Match changes in persistent
* Clean up warnings

## 2.6

* Atomic upsert support for postgreSQL backend

## 2.5

* changes for read/write typeclass split

## 2.2.2

* Postgresql primary key is Int4, not Int8 [#519](https://github.com/yesodweb/persistent/issues/519)

## 2.2.1.2

* Allow postgresql-simple 0.5

## 2.2.1.1

Query pg_catalog instead of information_schema for metadata.
This helps with permission issues as reported in issue #501

## 2.2.1

* Fix treatment of `NULL`s inside arrays.  For example, now you
  can use `array_agg` on a nullable column.

* New derived instances for `PostgresConf`: `Read`, `Data` and `Typeable`.

* New `mockMigration` function.  Works like `printMigration` but
  doesn't need a database connection.

* Fix typo on error message of the `FromJSON` instance of `PostgresConf`.

## 2.2

* Optimize the `insertMany` function to insert all rows and retrieve their keys in one SQL query. [#407](https://github.com/yesodweb/persistent/pull/407)

## 2.1.6

* Postgresql exceptions [#353](https://github.com/yesodweb/persistent/issues/353)

## 2.1.5.3

Migrations for custom primary keys

## 2.1.5.2

Support foreign key references to composite primary keys #389

## 2.1.5

* Allow timestamp value in database to be serialized (presumes UTC timezone) [Yesod #391](https://github.com/yesodweb/persistent/issues/391)

## 2.1.4

* Treat unknown extension types as PersistDbSpecific values [#385](https://github.com/yesodweb/persistent/pull/385)

## 2.1.3

* Added a `Show` instance for `PostgresConf`.
* `createPostgresqlPoolModified` added, see [relevant mailing list discussion](https://groups.google.com/d/msg/yesodweb/qUXrEN_swEo/O0pFwqwQIdcJ)

## 2.1.2.1

Documentation typo fix

## 2.1.1

Added `FromJSON` instance for `PostgresConf`.
