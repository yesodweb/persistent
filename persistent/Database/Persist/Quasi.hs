{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
This module defines the Persistent entity syntax used in the quasiquoter to generate persistent entities.

The basic structure of the syntax looks like this:

> TableName
>     fieldName      FieldType
>     otherField     String
>     nullableField  Int       Maybe

You start an entity definition with the table name, in this case, @TableName@. It's followed by a list of fields on the entity, which have the basic form @fieldName FieldType@. You can indicate that a field is nullable with 'Maybe' at the end of the type.

@persistent@ automatically generates an ID column for you, if you don't specify one, so the above table definition corresponds to the following SQL:

> CREATE TABLE table_name (
>     id                SERIAL PRIMARY KEY,
>     field_name        field_type NOT NULL,
>     other_field       varchar    NOT NULL,
>     nullable_field    int NULL
> );

Note that the exact SQL that is generated can be customized using the 'PersistSettings' that are passed to the 'parse' function.

It generates a Haskell datatype with the following form:

@
data TableName = TableName
    { tableNameFieldName :: FieldType
    , tableNameOtherField :: String
    , tableNameNullableField :: Maybe Int
    }
@

As with the SQL generated, the specifics of this are customizable.
See the "Database.Persist.TH" module for details.

= Deriving

You can add a deriving clause to a table, and the generated Haskell type will have a deriving clause with that.
Unlike normal Haskell syntax, you don't need parentheses or commas to separate the classes, and you can even have multiple deriving clauses.

> User
>     name String
>     age  Int
>     deriving Eq Show
>     deriving Ord

= Unique Keys

You can define a uniqueness key on a table with the following format:

> User
>    name String
>    age  Int
>
>    UniqueUserName name

This will put a unique index on the @user@ table and the @name@ field.

= Setting defaults

You can use a @default=${sql expression}@ clause to set a default for a field.
The thing following the @=@ is interpreted as SQL that will be put directly into the table definition.

@
User
    name    Text
    admin   Bool default=false
@

This creates a SQL definition like this:

> CREATE TABLE user (
>   id      SERIAL PRIMARY KEY,
>   name    VARCHAR NOT NULL,
>   admin   BOOL DEFAULT=false
> );

A restriction here is that you still need to provide a value when performing an @insert@, because the generated Haskell type has the form:

@
data User = User
    { userName :: Text
    , userAdmin :: Bool
    }
@

You can work around this by using a 'Maybe Bool' and supplying 'Nothing' by default.

__Note__: Persistent determines whether or not to migrate a column's default
value by comparing the exact string found in your @models@ file with the one
returned by the database. If a database canonicalizes the SQL @FALSE@ from your
@models@ file to @false@ in the database, Persistent will think the default
value needs to be migrated and
<https://github.com/yesodweb/persistent/issues/241 attempt a migration each time you start your app>.

To workaround this, find the exact SQL your DBMS uses for the default value. For example, using postgres:

@
psql database_name # Open postgres

\\d+ table_name -- describe the table schema
@

@
...
created       | timestamp without time zone | not null default now()
@

Then use the listed default value SQL inside your @models@ file.

= Custom ID column

If you don't want to use the default ID column type of 'Int64', you can set a custom type with an @Id@ field.
This @User@ has a @Text@ ID.

> User
>     Id   Text
>     name Text
>     age  Int

If you do this, it's a good idea to set a default for the ID.
Otherwise, you will need to use 'insertKey' instead of 'insert' when performing inserts.

@
'insertKey' (UserKey "Hello world!") (User "Bob" 32)
@

If you attempt to do @'insert' (User "Bob" 32)@, then you will receive a runtime error because the SQL database doesn't know how to make an ID for you anymore.
So instead just use a default expression, like this:

@
User
    Id      Text default=generate_user_id()
    name    Text
    age     Int
@

= Custom Primary Keys

Sometimes you don't want to have an ID column, and you want a different sort of primary key.
This is a table that stores unique email addresses, and the email is the primary key.
We store the first and second part (eg @first\@second@) separately.

@
Email
    firstPart   Text
    secondPart  Text

    Primary firstPart secondPart
@

This creates a table with the following form:

@
CREATE TABLE email (
    first_part  varchar,
    second_part varchar,

    PRIMARY KEY (first_part, second_part)
@

Since the primary key for this table is part of the record, it's called a "natural key" in the SQL lingo.
As a key with multiple fields, it is also a "composite key."

You can specify a @Primary@ key with a single field, too.

= Overriding SQL

You can use a @sql=custom@ annotation to provide some customization on the entity and field.
For example, you might prefer to name a table differently than what @persistent@ will do by default.
You may also prefer to name a field differently.

@
User sql=big_user_table
    fullName    String sql=name
    age         Int
@

This will alter the generated SQL to be:

@
CREATE TABEL big_user_table (
    id      SERIAL PRIMARY KEY,
    name    VARCHAR,
    age     INT
);
@

= Customizing Types/Tables

== JSON instances

You can automatically get ToJSON and FromJSON instances for any entity by adding @json@ to the entity line:

@
Person json
    name Text
@
Requires @\{\-\# LANGUAGE FlexibleInstances \#\-\}@

Customizable by using mpsEntityJSON
* http://hackage.haskell.org/package/persistent-template/docs/Database-Persist-TH.html#v:EntityJSON
* http://hackage.haskell.org/package/persistent/docs/Database-Persist-Class.html#v:keyValueEntityToJSON

== Changing table/collection name

@
Person sql=peoples
    name Text
@

== Change table/collection key definition (field name and\/or type, persistent >= 2.1)

@Id@ defines the column to use to define the key of the entity.
Without type, the default backend key type will be used. You can change its
database name using the @sql@ attributes :

@
Person
   Id         sql=my_id_name
   phone Text
@

With a Haskell type, the corresponding type is used. Note that you'll need to
use @default=@ to tell it what to do on insertion.

@
Person
   Id    Day default=CURRENT_DATE
   phone Text
@

@default=@ works for SQL databases, and is backend specific.
For MongoDB currently one always needs to create the key on the application
side and use @insertKey@. @insert@ will not work correctly. Sql backends can
also do this if default does not work.

@sqltype@ can also be used to specify a different database type

@
Currency
    Id String sqltype=varchar(3) sql=code
@

Composite key (using multiple columns) can also be defined using @Primary@.

@sql=@ also works for setting the names of unique indexes.

@
Person
  name Text
  phone Text
  UniquePersonPhone phone sql=UniqPerPhone
@

This makes a unique index requiring @phone@ to be unique across @Person@ rows.
Ordinarily Persistent will generate a snake-case index name from the
capitalized name provided such that @UniquePersonPhone@ becomes
@unique_person_phone@. However, we provided a @sql=@ so the index name in the
database will instead be @UniqPerPhone@. Keep in mind @sql=@ and @!@ attrs must
come after the list of fields in front of the index name in the quasi-quoter.



= Customizing Fields

== Nullable Fields

As illustrated in the example at the beginning of this page, we are able to represent nullable
fields by including 'Maybe' at the end of the type declaration:

> TableName
>     fieldName      FieldType
>     otherField     String
>     nullableField  Int       Maybe

Alternatively we can specify the keyword nullable:

> TableName
>     fieldName      FieldType
>     otherField     String
>     nullableField  Int       nullable

However the difference here is in the first instance the Haskell type will be 'Maybe Int',
but in the second it will be 'Int'. Be aware that this will cause runtime errors if the
database returns @NULL@ and the @PersistField@ instance does not handle @PersistNull@.

If you wish to define your Maybe types in a way that is similar to the actual Haskell
definition, you can define 'Maybe Int' like so:

> TableName
>     fieldName      FieldType
>     otherField     String
>     nullableField  (Maybe Int)

However, note, the field _must_ be enclosed in parenthesis.

== @sqltype=@

By default, Persistent maps the Haskell types you specify in the Models DSL to
an appropriate SQL type in the database (refer to the section "Conversion table
(migrations)" for the default mappings). Using the
@sqltype=@ option, you can  customize the SQL type Persistent uses for your
column. Use cases include:

* Interacting with an existing database whose column types don't match Persistent's defaults.
* Taking advantage of a specific SQL type's features
    * e.g. Using an equivalent type that has better space or performance characteristics

To use this setting, add the @sqltype=@ option after declaring your field name and type:

@
User
    username Text sqltype=varchar(255)
@

== Laziness

By default the records created by persistent have strict fields. You can prefix
a field name with @~@ to make it lazy (or @!@ to make it strict).

== Attributes

The QuasiQuoter allows you to provide arbitrary attributes to an entity or field.
This can be used to extend the code in ways that the library hasn't anticipated.
If you use this feature, we'd definitely appreciate hearing about it and
potentially supporting your use case directly!

@
User !funny
    field   String  !sad
    good    Dog     !sogood
@

We can see the attributes using the 'entityAttrs' field and the 'fieldAttrs' field.

@
userAttrs = do
    let userDefinition = 'entityDef' ('Proxy' :: 'Proxy' User)
    let userAttributes = 'entityAttrs' userDefinition
    let fieldAttributes = 'map' 'fieldAttrs' ('entityFields' userDefinition)
    print userAttributes
-- ["funny"]
    print fieldAttributes
-- [["sad"],["sogood"]]
@

== @MigrationOnly@

Introduced with @persistent-template@ 1.2.0. The purpose of this attribute is
to mark a field which will be entirely ignored by the normal processing, but
retained in the database definition for purposes of migration. This means, in
SQL, a column will not be flagged for removal by the migration scripts, even
though it is not used in your code. This is useful for phasing out usage of a
column before entirely removing it, or having columns which are needed by other
tools but not by Persistent.

@
Person
    name Text
    age Int
    unusedField ByteString Maybe MigrationOnly
@

Note that you almost certainly want to either mark the field as @Maybe@ or
provide a default value, otherwise insertions will fail.


== @SafeToRemove@

This is intended to be used as part of a deprecation of a field, after
@MigrationOnly@ has been used usually. This works somewhat as a superset of the
functionality of @MigrationOnly@. In addition, the field will be removed from
the database if it is present. Note that this is a destructive change which you
are marking as safe.

== Constraints

Migration will remove any manual constraints from your tables. Exception: constraints whose names begin with the string @__manual_@ (which starts with two underscores) will be preserved.


= Foreign Keys

If you define an entity and want to refer to it in another table, you can use the entity's Id type in a column directly.

@
Person
    name    Text

Dog
    name    Text
    owner   PersonId
@

This automatically creates a foreign key reference from @Dog@ to @Person@.
The foreign key constraint means that, if you have a @PersonId@ on the @Dog@, the database guarantees that the corresponding @Person@ exists in the database.
If you try to delete a @Person@ out of the database that has a @Dog@, you'll receive an exception that a foreign key violation has occurred.

== @constraint=@

You can use the @constraint=@ attribute to override the constraint name used in
migrations. This is useful particularly when the automatically generated
constraint names exceed database limits (e.g. MySQL does not allow constraint
names longer than 64 characters).

@
VeryLongTableName
  name Text

AnotherVeryLongTableName
  veryLongTableNameId VeryLongTableNameId constraint=short_foreign_key
@

== OnUpdate and OnDelete

These options affects how a referring record behaves when the target record is changed.
There are several options:

* 'Restrict' - This is the default. It prevents the action from occurring.
* 'Cascade' - this copies the change to the child record. If a parent record is deleted, then the child record will be deleted too.
* 'SetNull' - If the parent record is modified, then this sets the reference to @NULL@. This only works on @Maybe@ foreign keys.
* 'SetDefault' - This will set the column's value to the @default@ for the column, if specified.

To specify the behavior for a reference, write @OnUpdate@ or @OnDelete@ followed by the action.

@
Record
    -- If the referred Foo is deleted or updated, then this record will
    -- also be deleted or updated.
    fooId   FooId   OnDeleteCascade OnUpdateCascade

    -- If the referred Bar is deleted, then we'll set the reference to
    -- 'Nothing'. If the referred Bar is updated, then we'll cascade the
    -- update.
    barId   BarId Maybe     OnDeleteSetNull OnUpdateCascade

    -- If the referred Baz is deleted, then we set to the default ID.
    bazId   BazId   OnDeleteSetDefault  default=1
@

Let's demonstrate this with a shopping cart example.

@
User
    name    Text

Cart
    user    UserId Maybe

CartItem
    cartId  CartId
    itemId  ItemId

Item
    name    Text
    price   Int
@

Let's consider how we want to handle deletions and updates.
If a @User@ is deleted or update, then we want to cascade the action to the associated @Cart@.

@
Cart
    user    UserId Maybe OnDeleteCascade OnUpdateCascade
@

If an @Item@ is deleted, then we want to set the @CartItem@ to refer to a special "deleted item" in the database.
If a @Cart@ is deleted, though, then we just want to delete the @CartItem@.

@
CartItem
    cartId CartId   OnDeleteCascade
    itemId ItemId   OnDeleteSetDefault default=1
@

== @Foreign@ keyword

The above example is a "simple" foreign key. It refers directly to the Id column, and it only works with a non-composite primary key. We can define more complicated foreign keys using the @Foreign@ keyword.

A pseudo formal syntax for @Foreign@ is:

@
Foreign $(TargetEntity) [$(cascade-actions)] $(constraint-name) $(columns) [ $(references) ]

columns := column0 [column1 column2 .. columnX]
references := References $(target-columns)
target-columns := target-column0 [target-column1 target-columns2 .. target-columnX]
@

Columns are the columns as defined on this entity.
@target-columns@ are the columns as defined on the target entity.

Let's look at some examples.

=== Composite Primary Key References

The most common use for this is to refer to a composite primary key.
Since composite primary keys take up more than one column, we can't refer to them with a single @persistent@ column.

@
Email
    firstPart   Text
    secondPart  Text
    Primary firstPart secondPart

User
    name            Text
    emailFirstPart  Text
    emailSecondPart Text

    Foreign Email fk_user_email emailFirstPart emailSecondPart
@

If you omit the @References@ keyword, then it assumes that the foreign key reference is for the target table's primary key.
If we wanted to be fully redundant, we could specify the @References@ keyword.

@
    Foreign Email fk_user_email emailFirstPart emailSecondPart References firstPart secondPart
@

We can specify delete/cascade behavior directly after the target table.

@
    Foreign Email OnDeleteCascade OnUpdateCascade fk_user_email emailFirstPart emailSecondPart
@

Now, if the email is deleted or updated, the user will be deleted or updated to match.

=== Non-Primary Key References

SQL database backends allow you to create a foreign key to any column(s) with a Unique constraint.
Persistent does not check this, because you might be defining your uniqueness constraints outside of Persistent.
To do this, we must use the @References@ keyword.

@
User
    name    Text
    email   Text

    UniqueEmail email

Notification
    content Text
    sentTo  Text

    Foreign User fk_noti_user sentTo References email
@

If the target uniqueness constraint has multiple columns, then you must specify them independently.

@
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text

    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst emailSecond
@

= Documentation Comments

The quasiquoter supports ordinary comments with @--@ and @#@.
Since @persistent-2.10.5.1@, it also supports documentation comments.
The grammar for documentation comments is similar to Haskell's Haddock syntax, with a few restrictions:

1. Only the @-- | @ form is allowed.
2. You must put a space before and after the @|@ pipe character.
3. The comment must be indented at the same level as the entity or field it documents.

An example of the field documentation is:

@
-- | I am a doc comment for a User. Users are important
-- | to the application, and should be treasured.
User
    -- | Users have names. Call them by names.
    name String
    -- | A user can be old, or young, and we care about
    -- | this for some reason.
    age Int
@

The documentation is present on the @entityComments@ field on the @EntityDef@ for the entity:

@
>>> let userDefinition = entityDef (Proxy :: Proxy User)
>>> entityComments userDefinition
"I am a doc comment for a User. Users are important\nto the application, and should be treasured.\n"
@

Likewise, the field documentation is present in the @fieldComments@ field on the @FieldDef@ present in the @EntityDef@:

@
>>> let userFields = entityFields userDefinition
>>> let comments = map fieldComments userFields
>>> mapM_ putStrLn comments
"Users have names. Call them by names."
"A user can be old, or young, and we care about\nthis for some reason."
@

Unfortunately, we can't use this to create Haddocks for you, because <https://gitlab.haskell.org/ghc/ghc/issues/5467 Template Haskell does not support Haddock yet>.
@persistent@ backends *can* use this to generate SQL @COMMENT@s, which are useful for a database perspective, and you can use the <https://hackage.haskell.org/package/persistent-documentation @persistent-documentation@> library to render a Markdown document of the entity definitions.

= Sum types

== Field level

You'll frequently want to store an enum of values in your database. For
example, you might describe a @Person@'s employment status as being @Employed@,
@Unemployed@, or @Retired@. In Haskell this is represented with a sum type, and
Persistent provides a Template Haskell function to marshall these values to and
from the database:

@
-- @Employment.hs
{-# LANGUAGE TemplateHaskell #-}
module Employment where

import Database.Persist.TH
import Prelude

data Employment = Employed | Unemployed | Retired
    deriving (Show, Read, Eq)
derivePersistField "Employment"
@

@derivePersistField@ stores sum type values as strins in the database. While not as efficient as using integers, this approach simplifies adding and removing values from your enumeration.

Due to the GHC Stage Restriction, the call to the Template Haskell function @derivePersistField@ must be in a separate module than where the generated code is used.

Note: If you created a new module, make sure add it to the @exposed-modules@ section of your Cabal file.

Use the module by importing it into your @Model.hs@ file:

@
-- @Model.hs
import Employment
@

and use it in the @models@ DSL:

@
Person
    employment Employment
@

You can export the Employment module from Import to use it across your app:

@
-- @Import.hs
import Employment as Import
@

=== Entity-level

The
<https://github.com/yesodweb/persistent/blob/master/persistent-test/src/SumTypeTest.hs#L35 tests for this feature>
demonstrate their usage. Note the use of the sign @+@ in front of the entity
name.

The schema in the test is reproduced here:

@
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
@

Let's check out the definition of the Haskell type @Vehicle@.
Using @ghci@, we can query for @:info Vehicle@:

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

A @VehicleGeneric@ has two constructors:

- @VehicleBicycleSum@ with a @Key (BicycleGeneric backend)@ field
- @VehicleCarSum@ with a @Key (CarGeneric backend)@ field

The @Bicycle@ and @Car@ are typical @persistent@ entities.

This generates the following SQL migrations (formatted for readability):

@
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
@

The @vehicle@ table contains a nullable foreign key reference to both the bicycle and the car tables.

A SQL query that grabs all the vehicles from the database looks like this (note the @??@ is for the @persistent@ raw SQL query functions):

@
SELECT ??, ??, ??
FROM vehicle
LEFT JOIN car
    ON vehicle.car = car.id
LEFT JOIN bicycle
    ON vehicle.bicycle = bicycle.id
@

If we use the above query with @rawSql@, we'd get the following result:

@
getVehicles
    :: SqlPersistM
        [ ( Entity Vehicle
          , Maybe (Entity Bicycle)
          , Maybe (Entity Car)
          )
        ]
@

This result has some post-conditions that are not guaranteed by the types *or* the schema.
The constructor for @Entity Vehicle@ is going to determine which of the other members of the tuple is @Nothing@.
We can convert this to a friendlier domain model like this:

@
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
@

== Times with timezones

Storing times with timezones in one type in databases is not possible, although
it seems that it should be possible (@timezone@ and @timezonetz@ in
PostgreSQL). That's why starting with persistent 2.0, all times will be mapped
to @UTCTime@. If you need to store timezone information along with times in a
database, store the timezone in a second field. Here are some links about the
topic with further information:

* https://github.com/yesodweb/persistent/issues/290
* https://groups.google.com/forum/#!msg/yesodweb/MIfcV2bwM80/8QLFpgp1LykJ
* http://stackoverflow.com/questions/14615271/postgres-timestamp/14616640#14616640
* http://justatheory.com/computers/databases/postgresql/use-timestamptz.html
* https://github.com/lpsmith/postgresql-simple/issues/69
* https://github.com/nikita-volkov/hasql-postgres/issues/1

= Conversion table (migrations)

Here are the conversions between Haskell types and database types:

+------------+----------------------+-------------------+---------------+----------------+
| Haskell    | PostgreSQL           | MySQL             | MongoDB       |  SQLite        |
+============+======================+===================+===============+================+
| Text       |  VARCHAR             |  TEXT             | String        |  VARCHAR       |
+------------+----------------------+-------------------+---------------+----------------+
| ByteString |  BYTEA               |  BLOB             | BinData       |  BLOB          |
+------------+----------------------+-------------------+---------------+----------------+
| Int        |  INT8                |  BIGINT(20)       | NumberLong    |  INTEGER       |
+------------+----------------------+-------------------+---------------+----------------+
| Double     |  DOUBLE PRECISION    |  DOUBLE           | Double        |  REAL          |
+------------+----------------------+-------------------+---------------+----------------+
| Rational   |  NUMERIC(22, 12)     |  DECIMAL(32,20)   | *Unsupported* |  NUMERIC(32,20)|
+------------+----------------------+-------------------+---------------+----------------+
| Bool       |  BOOLEAN             |  TINYINT(1)       | Boolean       |  BOOLEAN       |
+------------+----------------------+-------------------+---------------+----------------+
| Day        |  DATE                |  DATE             | NumberLong    |  DATE          |
+------------+----------------------+-------------------+---------------+----------------+
| TimeOfDay  |  TIME                |  TIME\*\*         | *Unsupported* |  TIME          |
+------------+----------------------+-------------------+---------------+----------------+
| UTCTime\*  |  TIMESTAMP           |  DATETIME\*\*     | Date          |  TIMESTAMP     |
+------------+----------------------+-------------------+---------------+----------------+

Notes:

\* Support for @ZonedTime@ was dropped in persistent 2.0. @UTCTime@ can be used
with @timestamp without timezone@ and @timestamp with timezone@ in PostgreSQL.
See also the section "Times with timezones".

\*\* The default resolution for @TIME@ and @DATETIME@ in MySQL is one second.
As of MySQL version 5.6.4, and persistent-mysql-2.6.2, fractional seconds are
handled correctly if you declare an explicit precision by using @sqltype@. For
example, appending @sqltype=TIME(6)@ to a @TimeOfDay@ field definition will
give microsecond resolution.

= Compatibility tables

MySQL:

+-------------------+-----------------------------------------------------------------------+
|Haskell type       | Compatible MySQL types                                                |
+===================+=======================================================================+
| Bool              | Tiny                                                                  |
+-------------------+-----------------------------------------------------------------------+
| Int8              | Tiny                                                                  |
+-------------------+-----------------------------------------------------------------------+
| Int16             | Tiny,Short                                                            |
+-------------------+-----------------------------------------------------------------------+
| Int32             | Tiny,Short,Int24,Long                                                 |
+-------------------+-----------------------------------------------------------------------+
| Int               | Tiny,Short,Int24,Long,LongLong\*                                      |
+-------------------+-----------------------------------------------------------------------+
| Int64             | Tiny,Short,Int24,Long,LongLong                                        |
+-------------------+-----------------------------------------------------------------------+
| Integer           | Tiny,Short,Int24,Long,LongLong                                        |
+-------------------+-----------------------------------------------------------------------+
| Word8             | Tiny                                                                  |
+-------------------+-----------------------------------------------------------------------+
| Word16            | Tiny,Short                                                            |
+-------------------+-----------------------------------------------------------------------+
| Word32            | Tiny,Short,Int24,Long                                                 |
+-------------------+-----------------------------------------------------------------------+
| Word64            | Tiny,Short,Int24,Long,LongLong                                        |
| Double            | Float,Double,Decimal,NewDecimal,Tiny,Short,Int24,Long                 |
+-------------------+-----------------------------------------------------------------------+
| Ratio Integer     | Float,Double,Decimal,NewDecimal,Tiny,Short,Int24,Long,LongLong        |
+-------------------+-----------------------------------------------------------------------+
| ByteString        | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum   |
+-------------------+-----------------------------------------------------------------------+
| Lazy.ByteString   | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum   |
+-------------------+-----------------------------------------------------------------------+
| Encoding.Text\*\* | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum   |
+-------------------+-----------------------------------------------------------------------+
| Lazy.Text         | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum   |
+-------------------+-----------------------------------------------------------------------+
| [Char]/String     | VarChar,TinyBlob,MediumBlob,LongBlob,Blob,VarString,String,Set,Enum   |
+-------------------+-----------------------------------------------------------------------+
| UTCTime           | DateTime,Timestamp                                                    |
+-------------------+-----------------------------------------------------------------------+
| Day               | Year,Date,NewDate                                                     |
+-------------------+-----------------------------------------------------------------------+
| TimeOfDay         | Time                                                                  |
+-------------------+-----------------------------------------------------------------------+

\* When @Word@ size is 64bit

\*\* Utf8 only

Unsupported types:

+--------------------------------------------------------------------+
| Not currently supported                                            |
+====================================================================+
| Word                                                               |
+--------------------------------------------------------------------+
| Float                                                              |
+--------------------------------------------------------------------+
| Scientific <https://github.com/yesodweb/persistent/issues/225 #225>|
+--------------------------------------------------------------------+

See <http://hackage.haskell.org/package/mysql-simple/docs/Database-MySQL-Simple-Result.html MySQL.Simple.Result>.
-}
module Database.Persist.Quasi
    ( parse
    -- * 'PersistSettings'
    , PersistSettings
    , upperCaseSettings
    , lowerCaseSettings
    -- ** Getters and Setters
    , module Database.Persist.Quasi
    ) where

import Data.Text (Text)
import Database.Persist.Names
import Database.Persist.Quasi.Internal

-- | Retrieve the function in the 'PersistSettings' that modifies the names into
-- database names.
--
-- @since 2.13.0.0
getPsToDBName :: PersistSettings -> Text -> Text
getPsToDBName = psToDBName

-- | Set the name modification function that translates the QuasiQuoted names
-- for use in the database.
--
-- @since 2.13.0.0
setPsToDBName :: (Text -> Text) -> PersistSettings -> PersistSettings
setPsToDBName f ps = ps { psToDBName = f }

-- | Set a custom function used to create the constraint name
-- for a foreign key.
--
-- @since 2.13.0.0
setPsToFKName :: (EntityNameHS -> ConstraintNameHS -> Text) -> PersistSettings -> PersistSettings
setPsToFKName setter ps = ps { psToFKName = setter }

-- | A preset configuration function that puts an underscore
-- between the entity name and the constraint name when
-- creating a foreign key constraint name
--
-- @since 2.13.0.0
setPsUseSnakeCaseForiegnKeys :: PersistSettings -> PersistSettings
setPsUseSnakeCaseForiegnKeys = setPsToFKName (toFKNameInfixed "_")

-- | Retrieve whether or not the 'PersistSettings' will generate code with
-- strict fields.
--
-- @since 2.13.0.0
getPsStrictFields :: PersistSettings -> Bool
getPsStrictFields = psStrictFields

-- | Set whether or not the 'PersistSettings' will make fields strict.
--
-- @since 2.13.0.0
setPsStrictFields :: Bool -> PersistSettings -> PersistSettings
setPsStrictFields a ps = ps { psStrictFields = a }

-- | Retrieve the default name of the @id@ column.
--
-- @since 2.13.0.0
getPsIdName :: PersistSettings -> Text
getPsIdName = psIdName

-- | Set the default name of the @id@ column.
--
-- @since 2.13.0.0
setPsIdName :: Text -> PersistSettings -> PersistSettings
setPsIdName n ps = ps { psIdName = n }
