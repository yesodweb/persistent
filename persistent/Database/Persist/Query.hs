{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Persist.Query
  (   PersistQuery (..)
    , selectList
    , deleteCascadeWhere

    -- Internal
    , SelectOpt (..)
    , limitOffsetOrder
    , Filter (..)
    , PersistUpdate (..)
    , Update (..)
    , updateFieldName

    -- * query combinators
    , (=.), (+=.), (-=.), (*=.), (/=.)
    , (==.), (!=.), (<.), (>.), (<=.), (>=.)
    , (<-.), (/<-.)
    , (||.)
  ) where

import Database.Persist.Store

import Data.Enumerator hiding (consume, map)
import Data.Enumerator.List (consume)
import qualified Data.Enumerator.List as EL

import qualified Control.Monad.IO.Class as Trans
import qualified Control.Exception as E


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


class (PersistStore b m) => PersistQuery b m where
    -- | Update individual fields on a specific record.
    update :: PersistEntity val => Key b val -> [Update val] -> b m ()

    -- | Update individual fields on any record matching the given criterion.
    updateWhere :: PersistEntity val => [Filter val] -> [Update val] -> b m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: PersistEntity val => [Filter val] -> b m ()

    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    selectEnum
           :: PersistEntity val
           => [Filter val]
           -> [SelectOpt val]
           -> Enumerator (Key b val, val) (b m) a

    -- | get just the first record for the criterion
    selectFirst :: PersistEntity val
                => [Filter val]
                -> [SelectOpt val]
                -> b m (Maybe (Key b val, val))
    selectFirst filts opts = run_ $ selectEnum filts ((LimitTo 1):opts) ==<< EL.head


    -- | Get the 'Key's of all records matching the given criterion.
    selectKeys :: PersistEntity val
               => [Filter val]
               -> Enumerator (Key b val) (b m) a

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
           -> b m [(Key b val, val)]
selectList a b = do
    res <- run $ selectEnum a b ==<< consume
    case res of
        Left e -> Trans.liftIO . E.throwIO $ PersistError $ show e
        Right x -> return x

data SelectOpt v = forall typ. Asc (EntityField v typ)
                 | forall typ. Desc (EntityField v typ)
                 | OffsetBy Int
                 | LimitTo Int


deleteCascadeWhere :: (DeleteCascade a b m, PersistQuery b m)
                   => [Filter a] -> b m ()
deleteCascadeWhere filts = do
    res <- run $ selectKeys filts $ Continue iter
    case res of
        Left e -> Trans.liftIO . E.throwIO $ PersistError $ show e
        Right () -> return ()
  where
    iter EOF = Iteratee $ return $ Yield () EOF
    iter (Chunks keys) = Iteratee $ do
        mapM_ deleteCascade keys
        return $ Continue iter

data PersistUpdate = Assign | Add | Subtract | Multiply | Divide -- FIXME need something else here
    deriving (Read, Show, Enum, Bounded)

data Update v = forall typ. PersistField typ => Update
    { updateField :: EntityField v typ
    , updateValue :: typ
    , updateUpdate :: PersistUpdate -- FIXME Replace with expr down the road
    }

updateFieldName :: PersistEntity v => Update v -> String
updateFieldName (Update f _ _) = columnName $ persistColumnDef f

