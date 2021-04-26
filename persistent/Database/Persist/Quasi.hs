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
The thing following the `=` is interpreted as SQL that will be put directly into the table definition.

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

A restriction here is that you still need to provide a value when performing an `insert`, because the generated Haskell type has the form:

@
data User = User
    { userName :: Text
    , userAdmin :: Bool
    }
@

You can work around this by using a 'Maybe Bool' and supplying 'Nothing' by default.

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

= Attributes

The QuasiQuoter allows you to provide arbitrary attributes to an entity or field.
This can be used to extend the code in ways that the library hasn't anticipated.
If you use this feature, we'd definitely appreciate hearing about it and potentially supporting your use case directly!

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

The documentation is present on the `entityComments` field on the `EntityDef` for the entity:

@
>>> let userDefinition = entityDef (Proxy :: Proxy User)
>>> entityComments userDefinition
"I am a doc comment for a User. Users are important\nto the application, and should be treasured.\n"
@

Likewise, the field documentation is present in the `fieldComments` field on the `FieldDef` present in the `EntityDef`:

@
>>> let userFields = entityFields userDefinition
>>> let comments = map fieldComments userFields
>>> mapM_ putStrLn comments
"Users have names. Call them by names."
"A user can be old, or young, and we care about\nthis for some reason."
@

Unfortunately, we can't use this to create Haddocks for you, because <https://gitlab.haskell.org/ghc/ghc/issues/5467 Template Haskell does not support Haddock yet>.
`persistent` backends *can* use this to generate SQL @COMMENT@s, which are useful for a database perspective, and you can use the <https://hackage.haskell.org/package/persistent-documentation @persistent-documentation@> library to render a Markdown document of the entity definitions.

-}
module Database.Persist.Quasi
    ( parse
    -- * 'PersistSettings'
    , PersistSettings
    , upperCaseSettings
    , lowerCaseSettings
    , nullable
    -- ** Getters and Setters
    , module Database.Persist.Quasi
    ) where

import Data.Text (Text)
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

-- | Retrievce the default name of the @id@ column.
--
-- @since 2.13.0.0
getPsIdName :: PersistSettings -> Text
getPsIdName = psIdName

-- | Set the default name of the @id@ column.
--
-- @since 2.13.0.0
setPsIdName :: Text -> PersistSettings -> PersistSettings
setPsIdName n ps = ps { psIdName = n }
