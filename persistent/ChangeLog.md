# Changelog for persistent

## Unreleased

* [#1314](https://github.com/yesodweb/persistent/pull/1314)
    * Fix typos and minor documentation issues in Database.Persist and
      Database.Persist.Quasi.

## 2.13.1.2

* [#1308](https://github.com/yesodweb/persistent/pull/1308)
    * Consolidate the documentation for the Persistent quasiquoter in
      Database.Persist.Quasi.
* [#1312](https://github.com/yesodweb/persistent/pull/1312)
    * Reorganize documentation and link to more modules.
    * Expose `Database.Persist.Sql.Migration`

## 2.13.1.1

* [#1294](https://github.com/yesodweb/persistent/pull/1294)
    * Fix an issue where documentation comments on fields are in reverse line
      order.

## 2.13.1.0

* [#1264](https://github.com/yesodweb/persistent/pull/1264)
    * Support declaring Maybe before the type in model definitions

## 2.13.0.4

* [#1277](https://github.com/yesodweb/persistent/pull/1277)
    * Corrected the documentation of `addMigration` to match the actual
      behaviour - this will not change the behaviour of your code.

## 2.13.0.3

* [#1287](https://github.com/yesodweb/persistent/pull/1287)
    * Fix the duplicate entity check for transitive dependencies.
    * Fixes an issue where generating code would refer to the `ModelName` when
      making a reference to another table when the explicit code only refers to
      `ModelNameId`.

## 2.13.0.2

* [#1265](https://github.com/yesodweb/persistent/pull/1265)
    * Support GHC 9

## 2.13.0.1

* [#1268](https://github.com/yesodweb/persistent/pull/1268)
    * Show `keyFromValues` error

## 2.13.0.0

* [#1244](https://github.com/yesodweb/persistent/pull/1244)
    * Implement config for customising the FK name
* [#1252](https://github.com/yesodweb/persistent/pull/1252)
    * `mkMigrate` now defers to `mkEntityDefList` and `migrateModels` instead of
      fixing the foreign key references itself.
    * `mkSave` was deprecated - the function did not fix foreign key references.
      Please use `mkEntityDefList` instead.
    * `EntityDef` will now include fields marked `MigrationOnly` and
      `SafeToRemove`. Beforehand, those were filtered out, and `mkMigrate`
      applied. The function `getEntityFields` will only return fields defined on
      the Haskell type - for all columns, see `getEntityFieldsDatabase`.
* [#1225](https://github.com/yesodweb/persistent/pull/1225)
    * The fields and constructor for `SqlBackend` are no longer exported by
      default. They are available from an internal module,
      `Database.Persist.Sql.Types.Internal`. Breaking changes from `Internal`
      modules are not reflected in the major version. This will allow us to
      release new functionality without breaking your code. It's recommended to
      switch to using the smart constructor functions and setter functions that
      are now exported from `Database.Persist.Sql` instead.
    * A new API is available for constructing and using a `SqlBackend`, provided
      in `Database.Persist.SqlBackend`. Instead of using the `SqlBackend`
      directly, use `mkSqlBackend` and the datatype `MkSqlBackendArgs`. The
      `MkSqlBackendArgs` record has the same field names as the `SqlBackend`, so
      the translation is easy:

        ```diff
        - SqlBackend
        + mkSqlBackend MkSqlBackendArgs
            { connInsertSql = ...
            , connCommit = ...
            , connEscapeFieldName = ...
            , connEscapeTableName = ...
            , etc
            }
        ```

      Some fields were omitted in `MkSqlBackendArgs`. These fields are
      *optional* - they provide enhanced or backend-specific functionality. For
      these, use the setter functions like `setConnUpsertSql`.
    * Previously hidden modules are now exposed under the `Internal` namespace.
    * The `connLimitOffset` function used to have a `Bool` parameter. This
      parameter is unused and has been removed.
* [#1234](https://github.com/yesodweb/persistent/pull/1234)
    * You can now customize the default implied ID column. See the documentation
      in `Database.Persist.ImplicitIdDef` for more details.
    * Moved the various `Name` types into `Database.Persist.Names`
    * Removed the `hasCompositeKey` function. See `hasCompositePrimaryKey` and
      `hasNaturalKey` as replacements.
    * The `EntityDef` constructor and field labels are not exported by default.
      Get those from `Database.Persist.EntityDef.Internal`, but you should
      migrate to the getters/setters in `Database.Persist.EntityDef` as you can.
    * Added the `Database.Persist.FieldDef` and
      `Database.Persist.FieldDef.Internal` modules.
    * The `PersistSettings` type was made abstract. Please migrate to the
      getters/setters defined in that `Database.Persist.Quasi`, or use
      `Database.Persist.Quasi.Internal` if you don't mind the possibility of
      breaking changes.
    * Add the `runSqlCommand` function for running arbitrary SQL during
      migrations.
    * Add `migrateModels` function for a TH-free migration facility.
* [#1253](https://github.com/yesodweb/persistent/pull/1253)
    * Add `discoverEntities` to discover instances of the class and return their
      entity definitions.
* [#1250](https://github.com/yesodweb/persistent/pull/1250)
    * The `mpsGeneric` function has been deprecated. If you need this
      functionality, please comment with your needs on the GitHub issue tracker.
      We may un-deprecate it, or we may provide a new and better means of
      facilitating a solution to your problem.
* [#1255](https://github.com/yesodweb/persistent/pull/1255)
    * `mkPersist` now checks to see if an instance already exists for
      `PersistEntity` for the inputs.
* [#1256](https://github.com/yesodweb/persistent/pull/1256)
    * The QuasiQuoter has been refactored and improved.
    * You can now use `mkPersistWith` to pass in a list of pre-existing
      `EntityDef` to improve foreign key detection and splitting up models
      across multiple modules.
    * The `entityId` field now returns an `EntityIdDef`, which specifies what
      the ID field actually is. This is a move to better support natural keys.
    * Several types that had lists have been refactored to use nonempty lists to
      better capture the semantics.
    * `mkDeleteCascade` is deprecated. Please use the Cascade behavior directly
      on fields.
    * You can use `Key Foo` and `FooId` interchangeably in fields.
    * Support for GHC < 8.4 dropped.

## 2.12.1.2

* [#1258](https://github.com/yesodweb/persistent/pull/1258)
    * Support promoted types in Quasi Quoter
* [#1243](https://github.com/yesodweb/persistent/pull/1243)
    * Assorted cleanup of TH module
* [#1242](https://github.com/yesodweb/persistent/pull/1242)
    * Refactor setEmbedField to use do notation
* [#1237](https://github.com/yesodweb/persistent/pull/1237)
    * Remove nonEmptyOrFail function from recent tests

## 2.12.1.1

* [#1231](https://github.com/yesodweb/persistent/pull/1231)
    * Simplify Line type in Quasi module, always use NonEmpty
* [#1229](https://github.com/yesodweb/persistent/pull/1229)
    * The `#id` labels are now generated for entities.

## 2.12.1.0

* [#1218](https://github.com/yesodweb/persistent/pull/1218)
    * Refactoring name generating functions in TH
* [#1226](https://github.com/yesodweb/persistent/pull/1226)
    * Expose the `filterClause` and `filterClauseWithValues` functions to support
      the `upsertWhere` functionality in `persistent-postgresql`.

## 2.12.0.2

* [#1123](https://github.com/yesodweb/persistent/pull/1223)
    * Fix JSON encoding for `PersistValue`

## 2.12.0.1

* Refactoring token parsing in quasi module [#1206](https://github.com/yesodweb/persistent/pull/1206)
* Removing duplication from TH output [#1202](https://github.com/yesodweb/persistent/pull/1202)
* Refactor [] to NonEmpty in Quasi module [#1193](https://github.com/yesodweb/persistent/pull/1193)
* [#1162](https://github.com/yesodweb/persistent/pull/1162)
  * Replace `askLogFunc` with `askLoggerIO`
* Decomposed `HaskellName` into `ConstraintNameHS`, `EntityNameHS`, `FieldNameHS`. Decomposed `DBName` into `ConstraintNameDB`, `EntityNameDB`, `FieldNameDB` respectively. [#1174](https://github.com/yesodweb/persistent/pull/1174)
* Use `resourcet-pool` to break out some `Data.Pool` logic [#1163](https://github.com/yesodweb/persistent/pull/1163)
* [#1178](https://github.com/yesodweb/persistent/pull/1178)
  * Added 'withBaseBackend', 'withCompatible' to ease use of base/compatible backend queries in external code.
* Added GHC 8.2.2 and GHC 8.4.4 back into the CI and `persistent` builds on 8.2.2 again [#1181](https://github.com/yesodweb/persistent/issues/1181)
* [#1179](https://github.com/yesodweb/persistent/pull/1179)
  * Added `Compatible`, a newtype for marking a backend as compatible with another. Use it with `DerivingVia` to derive simple instances based on backend compatibility.
  * Added `makeCompatibleInstances` and `makeCompatibleKeyInstances`, TemplateHaskell invocations for auto-generating standalone derivations using `Compatible` and `DerivingVia`.
* [#1207](https://github.com/yesodweb/persistent/pull/1207)
    * @codygman discovered a bug in [issue #1199](https://github.com/yesodweb/persistent/issues/1199) where postgres connections were being returned to the `Pool SqlBackend` in an inconsistent state.
      @parsonsmatt debugged the issue and determined that it had something to do with asynchronous exceptions.
      Declaring it to be "out of his pay grade," he ripped the `poolToAcquire` function out and replaced it with `Data.Pool.withResource`, which doesn't exhibit the bug.
      Fortunately, this doesn't affect the public API, and can be a mere bug release.
    * Removed the functions `unsafeAcquireSqlConnFromPool`, `acquireASqlConnFromPool`, and `acquireSqlConnFromPoolWithIsolation`.
      For a replacement, see `runSqlPoolNoTransaction` and `runSqlPoolWithHooks`.
* Renaming values in persistent-template [#1203](https://github.com/yesodweb/persistent/pull/1203)
* [#1214](https://github.com/yesodweb/persistent/pull/1214):
    * Absorbed the `persistent-template` package. `persistent-template` will receive a 2.12 release with a warning and a deprecation notice.
    * Remove the `nooverlap` flag. It wasn't being used anymore.
* [#1205](https://github.com/yesodweb/persistent/pull/1205)
    * Introduce the `PersistLiteral_` constructor, replacing the `PersistLiteral`, `PersistLiteralEscaped`, and `PersistDbSpecific`.
    * The old constructors are now pattern synonyms. They don't actually differentiate between the various escaping strategies when consuming them! If you pattern match on multiple of `PersistDbSpecific`, `PersistLiteral`, or `PersistLiteralEscaped` , then you should use the `PersistLiteral_` constructor to differentiate between them.

## 2.11.0.2
* Fix a bug where an empty entity definition would break parsing of `EntityDef`s. [#1176](https://github.com/yesodweb/persistent/issues/1176)

## 2.11.0.1

* Docs/Bugs fixes [#1153](https://github.com/yesodweb/persistent/pull/1153)
  * Fix documentation on `FieldDef.fieldAttrs`.
  * Postgresql backend: Add a space in cascade clause of generated SQL.

## 2.11.0.0

* Foreign Key improvements [#1121](https://github.com/yesodweb/persistent/pull/1121)
  * It is now supported to refer to a table with an auto generated Primary Kay
  * It is now supported to refer to non-primary fields, using the keyword `References`
  * It is now supported to have cascade options for simple/single-field Foreign Keys
* Introduces a breaking change to the internal function `mkColumns`, which can now be passed a record of functions to override its default behavior. [#996](https://github.com/yesodweb/persistent/pull/996)
* Added explicit `forall` notation to make most API functions play nice when using `TypeApplications`. (e.g. instead of `selectList @_ @_ @User [] []`, you can now write `selectList @User [] []`) [#1006](https://github.com/yesodweb/persistent/pull/1006)
* [#1060](https://github.com/yesodweb/persistent/pull/1060)
  * The QuasiQuoter now supports `OnDelete` and `OnUpdate` cascade options.
* [#1044](https://github.com/yesodweb/persistent/pull/1044)
  * Field and constraint labels generated by TH can now be customized.
  * mpsPrefixFields is deprecated in favor of using these customisation functions.
* [#1032](https://github.com/yesodweb/persistent/pull/1032)
  * Instance for `Natural` is removed. See `OverflowNatural` for a
    replacement and rationale on why.
* [#1063](https://github.com/yesodweb/persistent/pull/1063)
  * A new class member `keyFromRecordM` allows you to construct a `Key
    record` from a `record` if it was defined with `Primary`.
* [#1036](https://github.com/yesodweb/persistent/pull/1036)
  * The method `entityIdFromJSON` that is used to parse entities now correctly works for entities that define a custom `Primary` key.
* [#856](https://github.com/yesodweb/persistent/pull/856)
  * Modify `upsertBy` to use backend-specific implementation (if any).
* [#1066](https://github.com/yesodweb/persistent/pull/1066)
  * You can set a column's `sql=id` for a non `Id` column.
* Fix a bug where unsafe migration error messages were being shown using `Show` prior to printing, resulting in less helpful output. [#1080](https://github.com/yesodweb/persistent/pull/1080)
* [#1087](https://github.com/yesodweb/persistent/pull/1087)
  * `RawSql` now has tuple instances up to GHC's max tuple size (62)
* [#1076](https://github.com/yesodweb/persistent/pull/1076)
  * `Loc` is now imported from `monad-logger` as opposed to `template-haskell`. Removes `template-haskell` as an explicit dependency.
* [#1114](https://github.com/yesodweb/persistent/pull/1114)
  * Remove unnecessary deriving of `Typeable`.
* [#1128](https://github.com/yesodweb/persistent/pull/1128)
  * Remove `Monad` constraint on `entityDef`
* [#1127](https://github.com/yesodweb/persistent/pull/1127)
  * Remove deriving of `Show` for uniques. Users that need a `Show` instance can put a standalone deriving instance:

    ```haskell
    deriving stock instance Show (Unique User)
    ```

* [#1131](https://github.com/yesodweb/persistent/pull/1131)
  * Add an `exists` function to the `PersistQueryRead` type class.
* [#1117](https://github.com/yesodweb/persistent/issues/1117)
  * Allow parsing UTCTimes from sqlite with the format "%F %T%Q" as well, instead of only "%FT%T%Q".
* [#1140](https://github.com/yesodweb/persistent/pull/1140)
  * A new function `checkUniqueUpdateable` allows you to check uniqueness
    constraints on an entity update without having to update it.
* [#1142](https://github.com/yesodweb/persistent/pull/1142)
    * Deprecate `hasCompositeKey` in favor of `hasCustomPrimaryKey` and `hasCompositePrimaryKey` functions.
* [#1098](https://github.com/yesodweb/persistent/pull/1098)
  * Add support for configuring the number of stripes and idle timeout for connection pools
    * For functions that do not specify an idle timeout, the default has been bumped to 600 seconds.
      * This change is based off the experience of two production codebases. See [#775](https://github.com/yesodweb/persistent/issues/775)
    * Add a new type `ConnectionPoolConfig` to configure the number of connections in a pool, their idle timeout, and stripe size.
    * Add `defaultConnectionPoolConfig` to create a `ConnectionPoolConfig`
    * Add `createSqlPoolWithConfig` and `withSqlPoolWithConfig`, which take this new data type
* [#1122](https://github.com/yesodweb/persistent/pull/1122), [#1152](https://github.com/yesodweb/persistent/pull/1152)
  * Adds a new constructor, `PersistLiteral ByteString` to `PersistValue` to support unescaped SQL literals.
    * Obviously, this is highly unsafe, and you should never use it with user input.
  * Adds a new field, `cGenerated :: Maybe Text` to `Column` for backend-specific support of generated columns.
    * Express generated fields in the Persistent DSL

    ```haskell
    GeneratedColumnExample
        fieldOne Text Maybe
        fieldTwo Text Maybe
        fieldThree Text Maybe generated=COALESCE(field_one,field_two)
    ```

    * Support for MySQL >= 5.7. (No version checking is performed! Using this feature with older versions of MySQL will cause runtime SQL exceptions!)
    * Support for Postgresql >= 12. (No version checking is performed! Using this feature with older versions of Postgresql will cause runtime SQL exceptions!)
    * Support for SQLite >= 3.31 (same caveat applies; support added in #1152 )
* [#1151](https://github.com/yesodweb/persistent/pull/1151)
  * Allow `OverloadedLabels` to be used with the `EntityField` type.

## 2.10.5.2

* [#1041](https://github.com/yesodweb/persistent/pull/1041)
  * Explicit foreign keys can now reference tables with custom sql name
  * Add qualified names to the stock classes list.

## 2.10.5.1

* [#1024](https://github.com/yesodweb/persistent/pull/1024)
    * Add the ability to do documentation comments in entity definition syntax. Unfortunately, TemplateHaskell cannot add documentation comments, so this can't be used to add Haddocks to entities.
    * Add Haddock explainers for some of the supported entity syntax in `Database.Persist.Quasi`

## 2.10.5

* Add the `EntityWithPrefix` type to allow users to specify a custom prefix for raw SQL queries. [#1018](https://github.com/yesodweb/persistent/pull/1018)
* Added Acquire based API to `Database.Persist.Sql` for working with
  connections/pools in monads which aren't MonadUnliftIO. [#984](https://github.com/yesodweb/persistent/pull/984)

## 2.10.4

* Log exceptions when closing a connection fails. See point 1 in [yesod #1635](https://github.com/yesodweb/yesod/issues/1635#issuecomment-547300856). [#978](https://github.com/yesodweb/persistent/pull/978)

## 2.10.3

* Added support for GHC 8.8 about MonadFail changes [#976](https://github.com/yesodweb/persistent/pull/976)

## 2.10.2

* Added `runMigrationQuiet` and `runMigrationUnsafeQuiet` to `Database.Persist.Sql.Migration` as safer alternatives to `runMigrationSilent`. [#971](https://github.com/yesodweb/persistent/pull/971)
  This functions as workaround/fix for: [#966](https://github.com/yesodweb/persistent/issues/966), [#948](https://github.com/yesodweb/persistent/issues/948), [#640](https://github.com/yesodweb/persistent/issues/640), and [#474](https://github.com/yesodweb/persistent/issues/474)
* Added RawSql instances for 9, 10, 11 and 12-column results. [#961](https://github.com/yesodweb/persistent/pull/961)

## 2.10.1

* Added `constraint=` attribute to allow users to specify foreign reference constraint names.

## 2.10.0

* Added two type classes `OnlyOneUniqueKey` and `AtLeastOneUniqueKey`. These classes are used as constraints on functions that expect a certain amount of unique keys. They are defined automatically as part of the `persistent-template`'s generation. [#885](https://github.com/yesodweb/persistent/pull/885)
* Add the `entityComments` field to the `EntityDef` datatype, and `fieldComments` fields to the `FieldDef` datatype. The QuasiQuoter does not currently know how to add documentation comments to these types, but it can be expanded later. [#865](https://github.com/yesodweb/persistent/pull/865)
* Expose the `SqlReadT` and `SqlWriteT` constructors. [#887](https://github.com/yesodweb/persistent/pull/887)
* Remove deprecated `Connection` type synonym. Please use `SqlBackend` instead. [#894](https://github.com/yesodweb/persistent/pull/894)
* Remove deprecated `SqlPersist` type synonym. Please use `SqlPersistT` instead. [#894](https://github.com/yesodweb/persistent/pull/894)
* Alter the type of `connUpsertSql` to take a list of unique definitions. This paves the way for more efficient upsert implementations. [#895](https://github.com/yesodweb/persistent/pull/895)

## 2.9.2

* Add documentation for the `Migration` type and some helpers. [#860](https://github.com/yesodweb/persistent/pull/860)

## 2.9.1

* Fix [#847](https://github.com/yesodweb/persistent/issues/847): SQL error with `putMany` on Sqlite when Entity has no unique index.

## 2.9.0

* Added support for SQL isolation levels to via SqlBackend. [#812]
* Move `Database.Persist.Sql.Raw.QQ` to a separate `persistent-qq` package [#827](https://github.com/yesodweb/persistent/issues/827)
* Fix [832](https://github.com/yesodweb/persistent/issues/832): `repsertMany` now matches `mapM_ (uncurry repsert)` and is atomic for supported sql back-ends.

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
