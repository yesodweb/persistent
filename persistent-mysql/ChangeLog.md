# Changelog for persistent-mysql

## (Unreleased) 2.10.3

* Compatibility with latest persistent

## 2.10.2.3

* Fix issue with multiple foreign keys on single column. [#1025](https://github.com/yesodweb/persistent/pull/1025)

## 2.10.2.2

* Compatibility with latest persistent-template for test suite [#1002](https://github.com/yesodweb/persistent/pull/1002/files)

## 2.10.2.1

* Changed persistent-mysql to use 'utf8mb4' instead of 'utf8' in migrations [#980](https://github.com/yesodweb/persistent/pull/980) @charukiewicz

## 2.10.2

* Added support for GHC 8.8 [#977](https://github.com/yesodweb/persistent/pull/977)

## 2.10.1

* Added `constraint=` attribute to allow users to specify foreign reference constraint names.

## 2.10.0

* Remove deprecated `SomeField` type and pattern synonym. Use `HandleUpdateCollision` type instead and the `copyField` function instead of `SomeField` constructor/pattern. [#894](https://github.com/yesodweb/persistent/pull/894)

## 2.9.0

* Added support for SQL isolation levels to via SqlBackend. [#812]
* Fix [832](https://github.com/yesodweb/persistent/issues/832): `repsertMany` now matches `mapM_ (uncurry repsert)` and is atomic.

## 2.8.1

* Implemented `connPutManySql` to utilize batched `putMany`. [#770](https://github.com/yesodweb/persistent/pull/770)

## 2.8.0

* Switch from `MonadBaseControl` to `MonadUnliftIO`
* Fix duplicate migrations when using `mediumtext`, `longtext`, `mediumblob`, `longblob`, and `double`s using a custom precision. [#754](https://github.com/yesodweb/persistent/pull/754)

-- This can be released as a minor change on the next update. Currently persistent-mysql can't be released because 2.6.2.2 depends on persistent-2.7.2 being released.

* The `SomeField` type was renamed to `HandleUpdateCollision` and deprecated. Please migrate to using `HandleUpdateCollision`.
* The `SomeField` constructor was deprecated, and a temporary pattern synonym introduced. Please migrate to using `copyField`.

## 2.6.2.2 [UNRELEASED ON HACKAGE]

-- This version depends on persistent 2.7.2, which introduced breaking changes and is deprecated on hackage.

* Fix ambiguous type errors introduced by `persistent-2.7.2` [#723](https://github.com/yesodweb/persistent/pull/723)

## 2.6.2.1

* Fix haddock documentation [#725](https://github.com/yesodweb/persistent/pull/725)

## 2.6.2

* Extend the `SomeField` type to allow `insertManyOnDuplicateKeyUpdate` to conditionally copy values.
* Depend on `mysql-simple >= 0.4.3` to fix encoding and decoding of date/time values with fractional seconds (when a column is specified using something like `sqltype=TIME(6)`).  See also [#705](https://github.com/yesodweb/persistent/issues/705)
* Fix behavior of `insertManyOnDuplicateKeyUpdate` to ignore duplicate key exceptions when no updates specified.

## 2.6.1

* Add functions `insertOnDuplicateKeyUpdate`, `insertManyOnDuplicateKeyUpdate` to `Database.Persist.MySQL` module.

## 2.6.0.2

Prevent spurious no-op migrations when `default=NULL` is specified - revised version [#672](https://github.com/yesodweb/persistent/pull/672) (which fixes bug [#671](https://github.com/yesodweb/persistent/issues/671) introduced by the earlier attempt [#641](https://github.com/yesodweb/persistent/pull/641))

## 2.6

Compatibility for backend-specific upsert functionality.
A lucky contributor could add upsert to the MySQL backend now, i.e.:
INSERT ... ON DUPICATE ...

## 2.5

* changes for read/write typeclass split

## 2.3.0.1

Support usign default= for changing the id field type

## 2.3

* Distinguish between binary and non-binary strings in MySQL [#451](https://github.com/yesodweb/persistent/pull/451)
	* Previously all string columns (VARCHAR, TEXT, etc.) were being returned from Persistent as `PersistByteString`s (i.e. as binary data). Persistent now checks character set information to determine if the value should be returned as `PersistText` or `PersistByteString`.
	* This is a **breaking change** if your code is relying on a `PersistByteString` being returned for string-like MySQL values; persistent-mysql itself had several runtime errors that needed to be fixed because of this patch. High-level code dealing purely with `PersistEntities` should be unaffected.

## 2.2

* Update to persistent 2.2

## 2.1.3

* Added a `Show` instance for `MySQLConf`.

## 2.1.2.1

Documentation typo fix

## 2.1.2

Provide a `FromJSON` instance for `MySQLConf`.
