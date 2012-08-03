{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.Persist.Query.Internal
  ( -- re-exported as public
      PersistQuery (..)
    , selectList
    , selectKeysList

    -- just Internal
    , SelectOpt (..)
    , limitOffsetOrder
    , Filter (..)
    , PersistUpdate (..)
    , Update (..)
    , updateFieldDef
    , deleteCascadeWhere

    -- * Exceptions
    , UpdateGetException (..)
  ) where

import Database.Persist.Store
import Database.Persist.EntityDef
import Control.Monad.Trans.Class (lift)
import Control.Monad.Base (liftBase)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

data UpdateGetException = KeyNotFound String
    deriving Typeable
instance Show UpdateGetException where
    show (KeyNotFound key) = "Key not found during updateGet: " ++ key
instance Exception UpdateGetException

class PersistStore backend m => PersistQuery backend m where
    -- | Update individual fields on a specific record.
    update :: PersistEntity val => Key backend val -> [Update val] -> backend m ()

    -- | Update individual fields on a specific record, and retrieve the
    -- updated value from the database.
    --
    -- Note that this function will throw an exception if the given key is not
    -- found in the database.
    updateGet :: PersistEntity val => Key backend val -> [Update val] -> backend m val
    updateGet key ups = do
        update key ups
        get key >>= maybe (liftBase $ throwIO $ KeyNotFound $ show key) return

    -- | Update individual fields on any record matching the given criterion.
    updateWhere :: PersistEntity val => [Filter val] -> [Update val] -> backend m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: PersistEntity val => [Filter val] -> backend m ()

    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    selectSource
           :: (PersistEntity val, PersistEntityBackend val ~ backend)
           => [Filter val]
           -> [SelectOpt val]
           -> C.Source (C.ResourceT (backend m)) (Entity val)

    -- | get just the first record for the criterion
    selectFirst :: (PersistEntity val, PersistEntityBackend val ~ backend)
                => [Filter val]
                -> [SelectOpt val]
                -> backend m (Maybe (Entity val))
    selectFirst filts opts = C.runResourceT
        $ selectSource filts ((LimitTo 1):opts) C.$$ CL.head


    -- | Get the 'Key's of all records matching the given criterion.
    selectKeys :: PersistEntity val
               => [Filter val]
               -> [SelectOpt val]
               -> C.Source (C.ResourceT (backend m)) (Key backend val)

    -- | The total number of records fulfilling the given criterion.
    count :: PersistEntity val => [Filter val] -> backend m Int

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


data SelectOpt v = forall typ. Asc (EntityField v typ)
                 | forall typ. Desc (EntityField v typ)
                 | OffsetBy Int
                 | LimitTo Int

-- | Call 'selectSource' but return the result as a list.
selectList :: (PersistEntity val, PersistQuery backend m, PersistEntityBackend val ~ backend)
           => [Filter val]
           -> [SelectOpt val]
           -> backend m [Entity val]
selectList a b = C.runResourceT $ selectSource a b C.$$ CL.consume

-- | Call 'selectKeys' but return the result as a list.
selectKeysList :: (PersistEntity val, PersistQuery b m, PersistEntityBackend val ~ b)
               => [Filter val]
               -> [SelectOpt val]
               -> b m [Key b val]
selectKeysList a b = C.runResourceT $ selectKeys a b C.$$ CL.consume

data PersistUpdate = Assign | Add | Subtract | Multiply | Divide -- FIXME need something else here
    deriving (Read, Show, Enum, Bounded)

data Update v = forall typ. PersistField typ => Update
    { updateField :: EntityField v typ
    , updateValue :: typ
    , updateUpdate :: PersistUpdate -- FIXME Replace with expr down the road
    }

limitOffsetOrder :: PersistEntity val => [SelectOpt val] -> (Int, Int, [SelectOpt val])
limitOffsetOrder opts =
    foldr go (0, 0, []) opts
  where
    go (LimitTo l) (_, b, c) = (l, b ,c)
    go (OffsetBy o) (a, _, c) = (a, o, c)
    go x (a, b, c) = (a, b, x : c)

updateFieldDef :: PersistEntity v => Update v -> FieldDef
updateFieldDef (Update f _ _) = persistFieldDef f

deleteCascadeWhere :: (DeleteCascade a backend m, PersistQuery backend m)
                   => [Filter a] -> backend m ()
deleteCascadeWhere filts = do
    C.runResourceT $ selectKeys filts [] C.$$ CL.mapM_ (lift . deleteCascade)
