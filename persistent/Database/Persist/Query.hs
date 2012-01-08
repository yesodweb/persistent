{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Persist.Query
  (   PersistQuery (..)
    , selectList
    , deleteCascadeWhere
    , Entity (..)

    -- Internal
    , SelectOpt (..)
    , limitOffsetOrder
    , Filter (..)
    , PersistUpdate (..)
    , Update (..)
    , updateFieldDef

    -- * query combinators
    , (=.), (+=.), (-=.), (*=.), (/=.)
    , (==.), (!=.), (<.), (>.), (<=.), (>=.)
    , (<-.), (/<-.)
    , (||.)
  ) where

import Database.Persist.Store
import Database.Persist.EntityDef

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

-- | Datatype that represents an entity, with both its key and
-- its Haskell representation.
--
-- When using the an SQL-based backend (such as SQLite or
-- PostgreSQL), an 'Entity' may take any number of columns
-- depending on how many fields it has. In order to reconstruct
-- your entity on the Haskell side, @persistent@ needs all of
-- your entity columns and in the right order.  Note that you
-- don't need to worry about this when using @persistent@\'s API
-- since everything is handled correctly behind the scenes.
--
-- However, if you want to issue a raw SQL command that returns
-- an 'Entity', then you have to be careful with the column
-- order.  While you could use @SELECT Entity.* WHERE ...@ and
-- that would work most of the time, there are times when the
-- order of the columns on your database is different from the
-- order that @persistent@ expects (for example, if you add a new
-- field in the middle of you entity definition and then use the
-- migration code -- @persistent@ will expect the column to be in
-- the middle, but your DBMS will put it as the last column).
-- So, instead of using a query like the one above, you may use
-- 'Database.Persist.GenericSql.rawSql' (from the
-- "Database.Persist.GenericSql" module) with its /entity
-- selection placeholder/ (a double question mark @??@).  Using
-- @rawSql@ the query above must be written as @SELECT ??  WHERE
-- ..@.  Then @rawSql@ will replace @??@ with the list of all
-- columns that we need from your entity in the right order.  If
-- your query returns two entities (i.e. @(Entity backend a,
-- Entity backend b)@), then you must you use @SELECT ??, ??
-- WHERE ...@, and so on.
data Entity backend entity =
    Entity { entityKey :: Key backend entity
           , entityVal :: entity }
    deriving (Eq, Ord, Show, Read)

infixr 3 =., +=., -=., *=., /=.
(=.), (+=.), (-=.), (*=.), (/=.) :: forall v typ.  PersistField typ => EntityField v typ -> typ -> Update v
-- | assign a field a value
f =. a = Update f a Assign
-- | assign a field by addition (+=)
f +=. a = Update f a Add
-- | assign a field by subtraction (-=)
f -=. a = Update f a Subtract
-- | assign a field by multiplication (*=)
f *=. a = Update f a Multiply
-- | assign a field by division (/=)
f /=. a = Update f a Divide

infix 4 ==., <., <=., >., >=., !=.
(==.), (!=.), (<.), (<=.), (>.), (>=.) ::
  forall v typ.  PersistField typ => EntityField v typ -> typ -> Filter v
f ==. a  = Filter f (Left a) Eq
f !=. a = Filter f (Left a) Ne
f <. a  = Filter f (Left a) Lt
f <=. a  = Filter f (Left a) Le
f >. a  = Filter f (Left a) Gt
f >=. a  = Filter f (Left a) Ge

infix 4 <-., /<-.
(<-.), (/<-.) :: forall v typ.  PersistField typ => EntityField v typ -> [typ] -> Filter v
-- | In
f <-. a = Filter f (Right a) In
-- | NotIn
f /<-. a = Filter f (Right a) NotIn

infixl 3 ||.
(||.) :: forall v. [Filter v] -> [Filter v] -> [Filter v]
-- | the OR of two lists of filters
a ||. b = [FilterOr  [FilterAnd a, FilterAnd b]]


class PersistStore b m => PersistQuery b m where
    -- | Update individual fields on a specific record.
    update :: PersistEntity val => Key b val -> [Update val] -> b m ()

    -- | Update individual fields on any record matching the given criterion.
    updateWhere :: PersistEntity val => [Filter val] -> [Update val] -> b m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: PersistEntity val => [Filter val] -> b m ()

    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    selectSource
           :: PersistEntity val
           => [Filter val]
           -> [SelectOpt val]
           -> C.Source (b m) (Entity b val)

    -- | get just the first record for the criterion
    selectFirst :: PersistEntity val
                => [Filter val]
                -> [SelectOpt val]
                -> b m (Maybe (Entity b val))
    selectFirst filts opts = C.runResourceT
        $ selectSource filts ((LimitTo 1):opts) C.$$ CL.head


    -- | Get the 'Key's of all records matching the given criterion.
    selectKeys :: PersistEntity val
               => [Filter val]
               -> C.Source (b m) (Key b val)

    -- | The total number of records fulfilling the given criterion.
    count :: PersistEntity val => [Filter val] -> b m Int

-- | Filters which are available for 'select', 'updateWhere' and
-- 'deleteWhere'. Each filter constructor specifies the field being
-- filtered on, the type of comparison applied (equals, not equals, etc)
-- and the argument for the comparison.
data Filter v = forall typ. PersistField typ => Filter
    { filterField  :: EntityField v typ
    , filterValue  :: Either typ [typ] -- FIXME
    , filterFilter :: PersistFilter -- FIXME
    }
    | FilterAnd [Filter v] -- ^ convenient for internal use, not needed for the API
    | FilterOr  [Filter v]


limitOffsetOrder :: PersistEntity val => [SelectOpt val] -> (Int, Int, [SelectOpt val])
limitOffsetOrder opts =
    foldr go (0, 0, []) opts
  where
    go (LimitTo l) (_, b, c) = (l, b ,c)
    go (OffsetBy o) (a, _, c) = (a, o, c)
    go x (a, b, c) = (a, b, x : c)

-- | Call 'select' but return the result as a list.
selectList :: (PersistEntity val, PersistQuery b m)
           => [Filter val]
           -> [SelectOpt val]
           -> b m [Entity b val]
selectList a b = C.runResourceT $ selectSource a b C.$$ CL.consume

data SelectOpt v = forall typ. Asc (EntityField v typ)
                 | forall typ. Desc (EntityField v typ)
                 | OffsetBy Int
                 | LimitTo Int


deleteCascadeWhere :: (DeleteCascade a b m, PersistQuery b m)
                   => [Filter a] -> b m ()
deleteCascadeWhere filts = do
    C.runResourceT $ selectKeys filts C.$$ CL.mapM_ deleteCascade

data PersistUpdate = Assign | Add | Subtract | Multiply | Divide -- FIXME need something else here
    deriving (Read, Show, Enum, Bounded)

data Update v = forall typ. PersistField typ => Update
    { updateField :: EntityField v typ
    , updateValue :: typ
    , updateUpdate :: PersistUpdate -- FIXME Replace with expr down the road
    }

updateFieldDef :: PersistEntity v => Update v -> FieldDef
updateFieldDef (Update f _ _) = persistFieldDef f
