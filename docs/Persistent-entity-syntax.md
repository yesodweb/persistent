# Persistent Entity Syntax

Persistent's entity file syntax.

## Conversion table (migrations)

Haskell	   |   PostgreSQL         |  MySQL            |  MongoDB      |  SQLite
-----------|----------------------|-------------------|---------------|---------
Text	   |  VARCHAR             |  TEXT             | String        |  VARCHAR
ByteString |  BYTEA               |  BLOB             | BinData       |  BLOB
Int        |  INT8                |  BIGINT(20)       | NumberLong    |  INTEGER
Double     |  DOUBLE PRECISION    |  DOUBLE           | Double        |  REAL
Rational   |  NUMERIC(22, 12)     |  DECIMAL(32,20)   | *Unsupported* |  NUMERIC(32,20)
Bool       |  BOOLEAN             |  TINYINT(1)       | Boolean       |  BOOLEAN
Day        |  DATE                |  DATE             | NumberLong    |  DATE
TimeOfDay  |  TIME                |  TIME\*\*         | *Unsupported* |  TIME
UTCTime\*  |  TIMESTAMP           |  DATETIME\*\*     | Date          |  TIMESTAMP

\* Support for `ZonedTime` was dropped in persistent 2.0. `UTCTime` can be used with `timestamp without timezone` and `timestamp with timezone` in PostgreSQL. See also [the section below about timezone support](#times-with-timezones).

\*\* The default resolution for `TIME` and `DATETIME` in MySQL is one second.  As of MySQL version 5.6.4, and persistent-mysql-2.6.2, fractional seconds are handled correctly if you declare an explicit precision by using [`sqltype`](#sqltype).  For example, appending `sqltype=TIME(6)` to a `TimeOfDay` field definition will give microsecond resolution.

## Compatibility tables

Haskell type    | Compatible MySQL types
----------------|--------------------------
Bool            | Tiny
Int8            | Tiny
Int16           | Tiny,Short
Int32           | Tiny,Short,Int24,Long
Int             | Tiny,Short,Int24,Long,LongLong\*
Int64           | Tiny,Short,Int24,Long,LongLong
Integer         | Tiny,Short,Int24,Long,LongLong
Word8           | Tiny
Word16          | Tiny,Short
Word32          | Tiny,Short,Int24,Long
Word64          | Tiny,Short,Int24,Long,LongLong
Double          | Float,Double,Decimal,NewDecimal,Tiny,Short,Int24,Long
Ratio Integer   | Float,Double,Decimal,NewDecimal,Tiny,Short,Int24,Long,LongLong
ByteString      | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum
Lazy.ByteString | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum
Encoding.Text\*\* | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum
Lazy.Text       | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum
[Char]/String   | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum
UTCTime         | DateTime,Timestamp
Day             | Year,Date,NewDate
TimeOfDay       | Time

\* When `Word` size is 64bit

\*\* Utf8 only

| Not currently supported
|-----------------------
| Word
| Float
| Scientific [#225](https://github.com/yesodweb/persistent/issues/225)

See [MySQL.Simple.Result](http://hackage.haskell.org/package/mysql-simple/docs/Database-MySQL-Simple-Result.html)


## Deriving

Persistent automatically derives some typeclasses, but the typeclasses derived can be changed.

```
Person
    name Text
    deriving Show Read
```


## JSON instances

You can automatically get ToJSON and FromJSON instances for any entity by adding `json` to the entity line:

```
Person json
    name Text
```
Requires `{-# LANGUAGE FlexibleInstances #-}`

Customizable by using mpsEntityJSON
* http://hackage.haskell.org/package/persistent-template/docs/Database-Persist-TH.html#v:EntityJSON
* http://hackage.haskell.org/package/persistent/docs/Database-Persist-Class.html#v:keyValueEntityToJSON

## Changing table/collection name

```
Person sql=peoples
    name Text
```


## Change table/collection key definition (field name and/or type ,persistent >= 2.1)

`Id` defines the column to use to define the key of the entity.
Without type, the default backend key type will be used. You can change it's database name using the `sql` attributes :

```
Person
   Id         sql=my_id_name
   phone Text
```
With a Haskell type, the corresponding type is used. Note that you'll need to use default= to tell it what to do on insertion.

```
Person
   Id    Day default=CURRENT_DATE
   phone Text
```

`default=` works for SQL databases, and is backend specific.
For MongoDB currently one always needs to create the key on the application side and use `insertKey`. `insert` will not work correctly. Sql backends can also do this if default does not work.

`sqltype` can also be used to specify a different database type

```Currency
    Id String sqltype=varchar(3) sql=code
```

Composite key (using multiple columns) can also be defined using `Primary` (see [Primary and Foreign Keys](#primary-and-foreign-keys)).

`sql=` also works for setting the names of unique indexes.

```
Person
  name Text
  phone Text
  UniquePersonPhone phone sql=UniqPerPhone
```

This makes a unique index requiring `phone` to be unique across `Person` rows. Ordinarily Persistent will generate a snake-case index name from the capitalized name provided such that `UniquePersonPhone` becomes `unique_person_phone`. However, we provided a `sql=` so the index name in the database will instead be `UniqPerPhone`. Keep in mind `sql=` and `!` attrs must come after the list of fields in front of the index name in the quasi-quoter.

## Primary and Foreign keys

The [tests for this feature](https://github.com/yesodweb/persistent/blob/master/persistent-test/src/CompositeTest.hs#L53) demonstrates their usage

### constraint=

You can use the `constraint=` attribute to override the constraint name used in migrations. This is useful particularly when the automatically generated constraint names exceed database limits (e.g. MySQL does not allow constraint names longer than 64 characters).

```
VeryLongTableName
  name Text

AnotherVeryLongTableName
  veryLongTableNameId VeryLongTableNameId constraint=short_foreign_key
```

## Laziness

By default the records created by persistent have strict fields. You can prefix a field name with `~` to make it lazy (or `!` to make it strict).


## Sum types

### Field level

You'll frequently want to store an enum of values in your database. For example, you might describe a `Person`'s employment status as being `Employed`, `Unemployed`, or `Retired`. In Haskell this is represented with a sum type, and Persistent provides a Template Haskell function to marshall these values to and from the database:

```haskell
-- @Employment.hs
{-# LANGUAGE TemplateHaskell #-}
module Employment where

import Database.Persist.TH
import Prelude

data Employment = Employed | Unemployed | Retired
    deriving (Show, Read, Eq)
derivePersistField "Employment"
```

`derivePersistField` stores sum type values as strings in the database. While not as efficient as using integers, this approach simplifies adding and removing values from your enumeration.

> Due to the GHC Stage Restriction, the call to the Template Haskell function `derivePersistField` must be in a separate module than where the generated code is used.

> Note: If you created a new module, make sure add it to the `exposed-modules` section of your Cabal file.

Use the module by importing it into your `Model.hs` file:

```haskell
-- @Model.hs
import Employment
```

and use it in the `models` DSL:

```
Person
    employment Employment
```

You can export the Employment module from Import to use it across your app:

```haskell
-- @Import.hs
import Employment as Import
```

### Entity-level

The [tests for this feature](https://github.com/yesodweb/persistent/blob/master/persistent-test/src/SumTypeTest.hs#L35) demonstrates their usage. Note the use of the sign `+` in front of the entity name.

The schema in the test is reproduced here:

```haskell
share [mkPersist persistSettings, mkMigrate "sumTypeMigrate"] [persistLowerCase|
Bicycle
    brand T.Text
Car
    make T.Text
    model T.Text
+Vehicle
    bicycle BicycleId
    car CarId
|]
```

Let's check out the definition of the Haskell type `Vehicle`.
Using `ghci`, we can query for `:info Vehicle`:

```
>>> :i Vehicle
type Vehicle = VehicleGeneric SqlBackend
        -- Defined at .../Projects/persistent/persistent-test/src/SumTypeTest.hs:26:1

>>> :i VehicleGeneric
type role VehicleGeneric nominal
data VehicleGeneric backend
  = VehicleBicycleSum (Key (BicycleGeneric backend))
  | VehicleCarSum (Key (CarGeneric backend))
        -- Defined at .../persistent/persistent-test/src/SumTypeTest.hs:26:1
-- lots of instances follow...
```

A `VehicleGeneric` has two constructors:

- `VehicleBicycleSum` with a `Key (BicycleGeneric backend)` field
- `VehicleCarSum` with a `Key (CarGeneric backend)` field

The `Bicycle` and `Car` are typical `persistent` entities.

This generates the following SQL migrations (formatted for readability):

```sql
CREATE TABLE "bicycle" (
    "id"        INTEGER PRIMARY KEY,
    "brand"     VARCHAR NOT NULL
);

CREATE TABLE "car"(
    "id"        INTEGER PRIMARY KEY,
    "make"      VARCHAR NOT NULL,
    "model"     VARCHAR NOT NULL
);

CREATE TABLE "vehicle"(
    "id"        INTEGER PRIMARY KEY,
    "bicycle"   INTEGER NULL REFERENCES "bicycle",
    "car"       INTEGER NULL REFERENCES "car"
);
```

The `vehicle` table contains a nullable foreign key reference to both the bicycle and the car tables.

A SQL query that grabs all the vehicles from the database looks like this (note the `??` is for the `persistent` raw SQL query functions):

```sql
SELECT ??, ??, ??
FROM vehicle
LEFT JOIN car
    ON vehicle.car = car.id
LEFT JOIN bicycle
    ON vehicle.bicycle = bicycle.id
```

If we use the above query with `rawSql`, we'd get the following result:

```haskell
getVehicles 
    :: SqlPersistM 
        [ ( Entity Vehicle
          , Maybe (Entity Bicycle)
          , Maybe (Entity Car)
          )
        ]
```

This result has some post-conditions that are not guaranteed by the types *or* the schema.
The constructor for `Entity Vehicle` is going to determine which of the other members of the tuple is `Nothing`.
We can convert this to a friendlier domain model like this:

```haskell
data Vehicle'
    = Car' Text Text
    | Bike Text

check = do
    result <- getVehicles
    pure (map convert result)

convert 
    :: (Entity Vehicle, Maybe (Entity Bicycle), Maybe (Entity Car))
    -> Vehicle'
convert (Entity _ (VehicycleBicycleSum _), Just (Entity _ (Bicycle brand)), _) =
    Bike brand
convert (Entity _ (VehicycleCarSum _), _, Just (Entity _ (Car make model))) =
    Car make model
convert _ =
    error "The database preconditions have been violated!"
```

## Printing Migrations

You can print migrations in GHCi with the following snippets:

```haskell
>>> :load SumTypeTest
>>> import qualified Data.Text as Text
>>> import Control.Monad.Logger (runStdoutLoggingT)
>>> import Database.Persist.Sqlite
>>> mapM_ Text.putStrLn =<< do runStdoutLoggingT $ runResourceT $ withSqliteConn ":memory:" (runSqlConn (showMigration sumTypeMigrate))
```

## sqltype=

By default, Persistent maps the Haskell types you specify in the Models DSL to an appropriate SQL type in the database (refer to the [conversion table](#conversion-table-migrations) above for the default mappings). Using the `sqltype=` option, you can  customize the SQL type Persistent uses for your column. Use cases include:

* Interacting with an existing database whose column types don't match Persistent's defaults.
* Taking advantage of a specific SQL type's features
    * e.g. Using an equivalent type that has better space or performance characteristics

To use this setting, add the `sqltype=` option after declaring your field name and type:

```
User
    username Text sqltype=varchar(255)
```


### Nullable fields

By default fields will have `NOT NULL` added. To allow `NULL` values, add `Maybe`.

    string Text Maybe

To add `DEFAULT NULL`

    string Text Maybe default=NULL


## default=

Persistent supports setting default values on SQL backends like so:

```
created UTCTime default=now()
```

The DEFAULT attribute is set with the exact SQL entered in your `models` file—Persistent performs *no analysis* of default values, and simply passes them onto the DBMS verbatim.

The DEFAULT attribute will affect migrations and raw SQL, but will have no impact on normal Persistent calls. You must still specify all fields, for example, when using an `insert` call.

**Note**: Persistent determines whether or not to migrate a column's default value by comparing the exact string found in your `models` file with the one returned by the database. If a database canonicalizes the SQL `FALSE` from your `models` file to `false` in the database, Persistent will think the default value needs to be migrated and [attempt a migration each time you start your app](https://github.com/yesodweb/persistent/issues/241).

To workaround this, find the exact SQL your DBMS uses for the default value. For example, using postgres:

```
psql database_name # Open postgres

\d+ table_name -- describe the table schema
```

```
...
created       | timestamp without time zone | not null default now()
```

Then use the listed default value SQL inside your `models` file.


## MigrationOnly

Introduced with `persistent-template` 1.2.0. The purpose of this attribute is to mark a field which will be entirely ignored by the normal processing, but retained in the database definition for purposes of migration. This means, in SQL, a column will not be flagged for removal by the migration scripts, even though it is not used in your code. This is useful for phasing out usage of a column before entirely removing it, or having columns which are needed by other tools but not by Persistent.

```
Person
    name Text
    age Int
    unusedField ByteString Maybe MigrationOnly
```

Note that you almost certainly want to either mark the field as `Maybe` or provide a default value, otherwise insertions will fail.


## SafeToRemove

This is intended to be used as part of a deprecation of a field, after `MigrationOnly` has been used usually. This works somewhat as a superset of the functionality of MigrationOnly. In addition, the field will be removed from the database if it is present. Note that this is a destructive change which you are marking as safe.


## Constraints

Migration will remove any manual constraints from your tables. Exception: constraints whose names begin with the string `__manual_` (which starts with two underscores) will be preserved.


### Uniqueness

Each uniqueness constraint must start with an uppercase letter (it does not have to be prefixed with 'Unique', that is just a convention), and is followed by one or more fields.

```
Person
    firstName Text
    lastName Text
    age Int
    UniqueFA firstName age
    UniqueL lastName
```

## Times with timezones

Storing times with timezones in one type in databases is not possible, although it seems that it should be possible (`timezone` and `timezonetz` in PostgreSQL). Thats why starting with persistent 2.0, all times will be mapped to `UTCTime`. If you need to store timezone information along with times in a database, store the timezone in a second field. Here are some links about the topic with further information:

* https://github.com/yesodweb/persistent/issues/290
* https://groups.google.com/forum/#!msg/yesodweb/MIfcV2bwM80/8QLFpgp1LykJ
* http://stackoverflow.com/questions/14615271/postgres-timestamp/14616640#14616640
* http://justatheory.com/computers/databases/postgresql/use-timestamptz.html
* https://github.com/lpsmith/postgresql-simple/issues/69
* https://github.com/nikita-volkov/hasql-postgres/issues/1

## Documentation Comments

The quasiquoter supports ordinary comments with `--` and `#`.
Since `persistent-2.10.5.1`, it also supports documentation comments.
The grammar for documentation comments is similar to Haskell's Haddock syntax, with a few restrictions:

1. Only the `-- | ` form is allowed.
2. You must put a space before and after the `|` pipe character.
3. The comment must be indented at the same level as the entity or field it documents.

An example of the field documentation is:

```
-- | I am a doc comment for a User. Users are important
-- | to the application, and should be treasured.
User
    -- | Users have names. Call them by names.
    name String
    -- | A user can be old, or young, and we care about 
    -- | this for some reason.
    age Int
```

The documentation is present on the `entityComments` field on the `EntityDef` for the entity:

```haskell
>>> let userDefinition = entityDef (Proxy :: Proxy User)
>>> entityComments userDefinition
"I am a doc comment for a User. Users are important\nto the application, and should be treasured.\n"
```

Likewise, the field documentation is present in the `fieldComments` field on the `FieldDef` present in the `EntityDef`:

```haskell
>>> let userFields = entityFields userDefinition
>>> let comments = map fieldComments userFields
>>> mapM_ putStrLn comments
"Users have names. Call them by names."
"A user can be old, or young, and we care about\nthis for some reason."
```

Unfortunately, we can't use this to create Haddocks for you, because [Template Haskell does not support Haddock yet](https://gitlab.haskell.org/ghc/ghc/issues/5467).
`persistent` backends *can* use this to generate SQL `COMMENT`s, which are useful for a database perspective, and you can use the [`persistent-documentation`](https://hackage.haskell.org/package/persistent-documentation) library to render a Markdown document of the entity definitions.
