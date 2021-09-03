{-# LANGUAGE ExistentialQuantification #-}

-- | Welcome to @persistent@!
--
-- This library intends to provide an easy, flexible, and convenient interface
-- to various data storage backends. Backends include SQL databases, like
-- @mysql@, @postgresql@, and @sqlite@, as well as NoSQL databases, like
-- @mongodb@ and @redis@.
--
-- If you intend on using a SQL database, then check out "Database.Persist.Sql".
module Database.Persist
    (
-- * Defining Database Models
--
-- | @persistent@ lets you define your database models using a special syntax.
-- This syntax allows you to customize the resulting Haskell datatypes and
-- database schema. See "Database.Persist.Quasi" for details on that definition
-- language.

-- ** Reference Schema & Dataset
--
-- | For a quick example of the syntax, we'll introduce this database schema, and
-- we'll use it to explain the update and filter combinators.
--
-- @
-- 'share' ['mkPersist' 'sqlSettings', 'mkMigrate' "migrateAll"] ['persistLowerCase'|
-- User
--     name String
--     age Int
--     deriving Show
-- |]
-- @
--
-- This creates a Haskell datatype that looks like this:
--
-- @
-- data User = User
--     { userName :: String
--     , userAge :: Int
--     }
--     deriving Show
-- @
--
-- In a SQL database, we'd get a migration like this:
--
-- @
-- CREATE TABLE "user" (
--      id    SERIAL PRIMARY KEY,
--      name  TEXT NOT NULL,
--      age   INT NOT NULL
-- );
-- @
--
-- The examples below will refer to this as dataset-1.
--
-- #dataset#
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |1    |SPJ  |40   |
-- > +-----+-----+-----+
-- > |2    |Simon|41   |
-- > +-----+-----+-----+

        -- * Database Operations
      -- | The module "Database.Persist.Class" defines how to operate with
        -- @persistent@ database models. Check that module out for basic
        -- operations, like 'get', 'insert', and 'selectList'.
      module Database.Persist.Class
      -- * Types
      -- | This module re-export contains a lot of the important types for
      -- working with @persistent@ datatypes and underlying values.
    , module Database.Persist.Types

      -- * Query Operators
      -- | A convention that @persistent@ tries to follow is that operators on
      -- Database types correspond to a Haskell (or database) operator with a @.@
      -- character at the end. So to do @a || b@ , you'd write @a '||.' b@. To

      -- ** Query update combinators
      -- | These operations are used when performing updates against the database.
      --  Functions like 'upsert' use them to provide new or modified values.
    , (=.), (+=.), (-=.), (*=.), (/=.)

      -- ** Query filter combinators
      -- | These functions are useful in the 'PersistQuery' class, like
      -- 'selectList', 'updateWhere', etc.
    , (==.), (!=.), (<.), (>.), (<=.), (>=.), (<-.), (/<-.), (||.)

      -- * JSON Utilities
    , listToJSON
    , mapToJSON
    , toJsonText
    , getPersistMap

      -- * Other utilities
    , limitOffsetOrder
    ) where

import Data.Aeson (toJSON, ToJSON)
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

import Database.Persist.Types
import Database.Persist.Class
import Database.Persist.Class.PersistField (getPersistMap)

infixr 3 =., +=., -=., *=., /=.
(=.), (+=.), (-=.), (*=.), (/=.) ::
  forall v typ.  PersistField typ => EntityField v typ -> typ -> Update v

-- | Assign a field a value.
--
-- === Examples
--
-- @
-- updateAge :: MonadIO m => ReaderT SqlBackend m ()
-- updateAge = updateWhere [UserName ==. \"SPJ\" ] [UserAge =. 45]
-- @
--
-- Similar to `updateWhere` which is shown in the above example you can use other functions present in the module "Database.Persist.Class". Note that the first parameter of `updateWhere` is [`Filter` val] and second parameter is [`Update` val]. By comparing this with the type of `==.` and `=.`, you can see that they match up in the above usage.
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+--------+
-- > |id   |name |age     |
-- > +-----+-----+--------+
-- > |1    |SPJ  |40 -> 45|
-- > +-----+-----+--------+
-- > |2    |Simon|41      |
-- > +-----+-----+--------+

f =. a = Update f a Assign

-- | Assign a field by addition (@+=@).
--
-- === Examples
--
-- @
-- addAge :: MonadIO m => ReaderT SqlBackend m ()
-- addAge = updateWhere [UserName ==. \"SPJ\" ] [UserAge +=. 1]
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+---------+
-- > |id   |name |age      |
-- > +-----+-----+---------+
-- > |1    |SPJ  |40 -> 41 |
-- > +-----+-----+---------+
-- > |2    |Simon|41       |
-- > +-----+-----+---------+


f +=. a = Update f a Add

-- | Assign a field by subtraction (@-=@).
--
-- === Examples
--
-- @
-- subtractAge :: MonadIO m => ReaderT SqlBackend m ()
-- subtractAge = updateWhere [UserName ==. \"SPJ\" ] [UserAge -=. 1]
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+---------+
-- > |id   |name |age      |
-- > +-----+-----+---------+
-- > |1    |SPJ  |40 -> 39 |
-- > +-----+-----+---------+
-- > |2    |Simon|41       |
-- > +-----+-----+---------+

f -=. a = Update f a Subtract

-- | Assign a field by multiplication (@*=@).
--
-- === Examples
--
-- @
-- multiplyAge :: MonadIO m => ReaderT SqlBackend m ()
-- multiplyAge = updateWhere [UserName ==. \"SPJ\" ] [UserAge *=. 2]
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+--------+
-- > |id   |name |age     |
-- > +-----+-----+--------+
-- > |1    |SPJ  |40 -> 80|
-- > +-----+-----+--------+
-- > |2    |Simon|41      |
-- > +-----+-----+--------+


f *=. a = Update f a Multiply

-- | Assign a field by division (@/=@).
--
-- === Examples
--
-- @
-- divideAge :: MonadIO m => ReaderT SqlBackend m ()
-- divideAge = updateWhere [UserName ==. \"SPJ\" ] [UserAge /=. 2]
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+---------+
-- > |id   |name |age      |
-- > +-----+-----+---------+
-- > |1    |SPJ  |40 -> 20 |
-- > +-----+-----+---------+
-- > |2    |Simon|41       |
-- > +-----+-----+---------+

f /=. a = Update f a Divide

infix 4 ==., <., <=., >., >=., !=.
(==.), (!=.), (<.), (<=.), (>.), (>=.) ::
  forall v typ.  PersistField typ => EntityField v typ -> typ -> Filter v

-- | Check for equality.
--
-- === Examples
--
-- @
-- selectSPJ :: MonadIO m => ReaderT SqlBackend m [Entity User]
-- selectSPJ = selectList [UserName ==. \"SPJ\" ] []
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |1    |SPJ  |40   |
-- > +-----+-----+-----+

f ==. a  = Filter f (FilterValue a) Eq

-- | Non-equality check.
--
-- === Examples
--
-- @
-- selectSimon :: MonadIO m => ReaderT SqlBackend m [Entity User]
-- selectSimon = selectList [UserName !=. \"SPJ\" ] []
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |2    |Simon|41   |
-- > +-----+-----+-----+

f !=. a = Filter f (FilterValue a) Ne

-- | Less-than check.
--
-- === Examples
--
-- @
-- selectLessAge :: MonadIO m => ReaderT SqlBackend m [Entity User]
-- selectLessAge = selectList [UserAge <. 41 ] []
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |1    |SPJ  |40   |
-- > +-----+-----+-----+

f <. a  = Filter f (FilterValue a) Lt

-- | Less-than or equal check.
--
-- === Examples
--
-- @
-- selectLessEqualAge :: MonadIO m => ReaderT SqlBackend m [Entity User]
-- selectLessEqualAge = selectList [UserAge <=. 40 ] []
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |1    |SPJ  |40   |
-- > +-----+-----+-----+

f <=. a  = Filter f (FilterValue a) Le

-- | Greater-than check.
--
-- === Examples
--
-- @
-- selectGreaterAge :: MonadIO m => ReaderT SqlBackend m [Entity User]
-- selectGreaterAge = selectList [UserAge >. 40 ] []
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |2    |Simon|41   |
-- > +-----+-----+-----+

f >. a  = Filter f (FilterValue a) Gt

-- | Greater-than or equal check.
--
-- === Examples
--
-- @
-- selectGreaterEqualAge :: MonadIO m => ReaderT SqlBackend m [Entity User]
-- selectGreaterEqualAge = selectList [UserAge >=. 41 ] []
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |2    |Simon|41   |
-- > +-----+-----+-----+

f >=. a  = Filter f (FilterValue a) Ge

infix 4 <-., /<-.
(<-.), (/<-.) :: forall v typ.  PersistField typ => EntityField v typ -> [typ] -> Filter v

-- | Check if value is in given list.
--
-- === Examples
--
-- @
-- selectUsers :: MonadIO m => ReaderT SqlBackend m [Entity User]
-- selectUsers = selectList [UserAge <-. [40, 41]] []
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |1    |SPJ  |40   |
-- > +-----+-----+-----+
-- > |2    |Simon|41   |
-- > +-----+-----+-----+
--
--
-- @
-- selectSPJ :: MonadIO m => ReaderT SqlBackend m [Entity User]
-- selectSPJ = selectList [UserAge <-. [40]] []
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |1    |SPJ  |40   |
-- > +-----+-----+-----+

f <-. a = Filter f (FilterValues a) In

-- | Check if value is not in given list.
--
-- === Examples
--
-- @
-- selectSimon :: MonadIO m => ReaderT SqlBackend m [Entity User]
-- selectSimon = selectList [UserAge /<-. [40]] []
-- @
--
-- The above query when applied on <#dataset dataset-1>, will produce this:
--
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |2    |Simon|41   |
-- > +-----+-----+-----+

f /<-. a = Filter f (FilterValues a) NotIn

infixl 3 ||.
(||.) :: forall v. [Filter v] -> [Filter v] -> [Filter v]

-- | The OR of two lists of filters. For example:
--
-- > selectList
-- >     ([ PersonAge >. 25
-- >      , PersonAge <. 30 ] ||.
-- >      [ PersonIncome >. 15000
-- >      , PersonIncome <. 25000 ])
-- >     []
--
-- will filter records where a person's age is between 25 and 30 /or/ a
-- person's income is between (15000 and 25000).
--
-- If you are looking for an @(&&.)@ operator to do @(A AND B AND (C OR D))@
-- you can use the @(++)@ operator instead as there is no @(&&.)@. For
-- example:
--
-- > selectList
-- >     ([ PersonAge >. 25
-- >      , PersonAge <. 30 ] ++
-- >     ([PersonCategory ==. 1] ||.
-- >      [PersonCategory ==. 5]))
-- >     []
--
-- will filter records where a person's age is between 25 and 30 /and/
-- (person's category is either 1 or 5).
a ||. b = [FilterOr  [FilterAnd a, FilterAnd b]]

-- | Convert list of 'PersistValue's into textual representation of JSON
-- object. This is a type-constrained synonym for 'toJsonText'.
listToJSON :: [PersistValue] -> T.Text
listToJSON = toJsonText

-- | Convert map (list of tuples) into textual representation of JSON
-- object. This is a type-constrained synonym for 'toJsonText'.
mapToJSON :: [(T.Text, PersistValue)] -> T.Text
mapToJSON = toJsonText

-- | A more general way to convert instances of `ToJSON` type class to
-- strict text 'T.Text'.
toJsonText :: ToJSON j => j -> T.Text
toJsonText = toStrict . toLazyText . encodeToTextBuilder . toJSON

-- | FIXME What's this exactly?
limitOffsetOrder :: PersistEntity val
  => [SelectOpt val]
  -> (Int, Int, [SelectOpt val])
limitOffsetOrder opts =
    foldr go (0, 0, []) opts
  where
    go (LimitTo l) (_, b, c) = (l, b ,c)
    go (OffsetBy o) (a, _, c) = (a, o, c)
    go x (a, b, c) = (a, b, x : c)
