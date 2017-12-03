## 2.6.5

* Exports the record update functions for `SqliteConnectionInfo`
	* These were previously only available as lenses.

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

* Update `sqlite` cbit sources to 3.19.3 from 3.12.1

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
