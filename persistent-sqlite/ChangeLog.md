# Changelog for persistent-sqlite

## (Unreleased) 2.11.0.0

* [#1060](https://github.com/yesodweb/persistent/pull/1060)
  * The QuasiQuoter now supports `OnDelete` and `OnUpdate` cascade options.

## 2.10.6.2

* Move template haskell splices to be correct (and GHC 8.10 compatible) [#1034](https://github.com/yesodweb/persistent/pull/1034)

## 2.10.6.1

* Missing `includes` and `install-includes` cabal fields added, to allow
  packages depending on persistent-sqlite access to the header files.

## 2.10.6

* Bump SQLite amalgamation to version 3.30.1 [#991](https://github.com/yesodweb/persistent/pull/991)
* Add `createRawSqlitePoolFromInfo`, `createRawSqlitePoolFromInfo_`,
  `withRawSqlitePoolInfo`, and `withRawSqlitePoolInfo_` to match the existing
  pool functions for regular `SqlBackend`. [#983](https://github.com/yesodweb/persistent/pull/983)
>>>>>>> master

## 2.10.5.2

* Compatibility with latest persistent-template for test suite [#1002](https://github.com/yesodweb/persistent/pull/1002/files)

## 2.10.5.1

* a fix for template-haskell 2.16, GHC 8.10 alpha [#993](https://github.com/yesodweb/persistent/pull/993) @simonmichael

## 2.10.5

* Foreign keys table constraints are correctly generated [#945](https://github.com/yesodweb/persistent/pull/945) @kderme

## 2.10.4

* Fix bug with 2.10.3 and 2.10.2 that caused the `RawSqlite` loop. [#934](https://github.com/yesodweb/persistent/pull/934) @merijn

## 2.10.3

* Unique constraints are correctly generated. [#922](https://github.com/yesodweb/persistent/pull/922) @kderme

## 2.10.2

* Add a new `RawSqlite` type and `withRawSqliteConnInfo` function that allow  access to the underlying Sqlite `Connection` type. [#772](https://github.com/yesodweb/persistent/pull/772)
* Expose the internals of `Connection` in an Internal module, allowing the user to call SQLite functions via the C FFI. [#772](https://github.com/yesodweb/persistent/pull/772)
* Add a flag for SQLITE_STAT4 and enable it by default, allowing for better query optimisation when using ANALYZE. This breaks the query planner stability guarantee, but the required flag for that isn't enabled or exposed by persistent. Only affects the vendored SQLite library, has no effect when using system SQLite.
* Add support for migrating entities with composite primary keys. Fixes [#669](https://github.com/yesodweb/persistent/issues/669)
* Fix a bug when using the `Filter` datatype directly. See [#915](https://github.com/yesodweb/persistent/pull/915) for more details.

## 2.10.1

* Add support for reading text values with null characters from the database. Fixes [#921](https://github.com/yesodweb/persistent/issues/921)

## 2.10.0

* Updated for `persistent-2.10.0` compatibility.

## 2.9.3

* Add retry-on-busy support, automatically retrying when sqlite returns a busy
  error on enabling WAL mode, and providing helper `retryOnBusy` and
  `waitForDatabase` identifiers.

## 2.9.2

* Add enableExtendedResultCodes and disableExtendedResultCodes functions

## 2.9.1

* Bump vendored SQLite library to [3.26.0](https://www.sqlite.org/releaselog/3_26_0.html) to address [RCE bug: `magellan`](https://blade.tencent.com/magellan/index_en.html).

## 2.9.0

* Added support for SQL isolation levels to via SqlBackend. [#812] SQLite technically only supports Serializable.
* Update the vendored SQLite C library from 3.22.0 to 3.25.2. See [the SQLite changelog](https://sqlite.org/changes.html) for details.
* Fix [832](https://github.com/yesodweb/persistent/issues/832): `repsertMany` now matches `mapM_ (uncurry repsert)` and is atomic.

## 2.8.2

* Add the `extraPragmas` setting

## 2.8.1.2

* Add flag to enable full-text search extensions (enabled by default)
* Add flag to enable URI filename support (enabled by default)
* Add flag to enable using usleep (enabled by default)
  - Enabling usleep allows sqlite to use a finer granularity when sleeping (reduces time between locks)
* Add flag to enable json1 extension (enabled by default)

## 2.8.1.1

* Update the vendored SQLite C library from 3.19.3 to 3.22.0. See [the SQLite changelog](https://sqlite.org/changes.html) for details.

## 2.8.1

* Updated `SqlBackend` definition to set `connPutManySql`. [#770](https://github.com/yesodweb/persistent/pull/770)

## 2.8.0

* Switch from `MonadBaseControl` to `MonadUnliftIO`

## 2.6.4

* Adds a new function `stepConn`, which uses an additional parameter to give more detailed error messages [#750](https://github.com/yesodweb/persistent/pull/750)
* Restores the previous function signature of `step`, which was accidentally changed in 2.6.3.2

## 2.6.3.2

* This release accidentally broke API, and is deprecated on Hackage.
* Provide more detailed error messages when using the `step` function [#730](https://github.com/yesodweb/persistent/pull/730)

## 2.6.3.1

* Fix migration to avoid creating foreign-key constraints in temporary tables [#736](https://github.com/yesodweb/persistent/pull/736)

## 2.6.3

* Add 'use-pkgconfig' flag to use pkg-config to find system SQLite library.

## 2.6.2.1

* Update the vendored SQLite C library from 3.12.1 to 3.19.3. See [the SQLite changelog](https://sqlite.org/changes.html) for details.

## 2.6.2

* Turned on foreign key constraints [#646](https://github.com/yesodweb/persistent/issues/646)
* Added new `SqliteConnectionInfo`-based API

## 2.6.1

* Added functions to monitor (status) and control (softHeapLimit) process-wide SQLite memory usage.

## 2.6.0.1

* Ensure connection is closed if wrapConnectionWal fails

## 2.6

Compatibility for backend-specific upsert functionality.
A lucky contributor could try to add upsert to the sqlite backend now.
It would definitely be tricky though because sqlite does not really have this
feature.
http://stackoverflow.com/questions/418898/sqlite-upsert-not-insert-or-replace/4330694#4330694

## 2.5.0.2

* Fix lstat workaround for https://ghc.haskell.org/trac/ghc/ticket/7072 on Mac OS X [#564](https://github.com/yesodweb/persistent/pull/564)

## 2.5.0.1

* Reapply workaround for https://ghc.haskell.org/trac/ghc/ticket/7072 [#561](https://github.com/yesodweb/persistent/pull/561)

## 2.5

* changes for read/write typeclass split

## 2.2.2

* Upgrade to SQLite 3.12.1 [#551](https://github.com/yesodweb/persistent/issues/551)

## 2.2.1

* Upgrade to SQLite 3.8.11.1 [#444](https://github.com/yesodweb/persistent/pull/444)

## 2.2

Update to persistent 2.2

## 2.1.4.1

* Add missing source files [#382](https://github.com/yesodweb/persistent/issues/382)

## 2.1.4

* Add log support to persistent-sqlite [#381](https://github.com/yesodweb/persistent/pull/381)

## 2.1.3

* Added a `Show` instance for `SqliteConf`.
* Use `SqliteException` instead of calling `fail` [#364](https://github.com/yesodweb/persistent/issues/364)

## 2.1.2

* Turn on write-ahead log [#363](https://github.com/yesodweb/persistent/issues/363)
    * Prepending `WAL=off ` to your connection string will recover the previous behavior.

## 2.1.1.1

Fix rendering of `UTCTime` to match SQLite requirements (see [issue
#328](https://github.com/yesodweb/persistent/issues/328#issuecomment-65887577)).

## 2.1.1

Provide a `FromJSON` instance for `SqliteConf`.
