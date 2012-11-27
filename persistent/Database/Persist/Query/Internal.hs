{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
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

    -- * Filters
    , BackendSpecificFilter
  ) where

import Database.Persist.Store
import Database.Persist.EntityDef
import Control.Monad.IO.Class (liftIO)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Control.Monad.Trans.Class (lift)
import Data.Monoid (Monoid)

import Data.Conduit (Pipe)
import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error   )
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import Control.Monad.Trans.Resource ( ResourceT)

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

data UpdateGetException = KeyNotFound String
    deriving Typeable
instance Show UpdateGetException where
    show (KeyNotFound key) = "Key not found during updateGet: " ++ key
instance Exception UpdateGetException

class PersistStore m => PersistQuery m where
    -- | Update individual fields on a specific record.
    update :: (PersistEntity val, PersistEntityBackend val ~ PersistMonadBackend m)
           => Key' val -> [Update val] -> m ()

    -- | Update individual fields on a specific record, and retrieve the
    -- updated value from the database.
    --
    -- Note that this function will throw an exception if the given key is not
    -- found in the database.
    updateGet :: (PersistEntity val, PersistMonadBackend m ~ PersistEntityBackend val)
              => Key' val -> [Update val] -> m val
    updateGet key ups = do
        update key ups
        get key >>= maybe (liftIO $ throwIO $ KeyNotFound $ show key) return

    -- | Update individual fields on any record matching the given criterion.
    updateWhere :: (PersistEntity val, PersistEntityBackend val ~ PersistMonadBackend m)
                => [Filter val] -> [Update val] -> m ()

    -- | Delete all records matching the given criterion.
    deleteWhere :: (PersistEntity val, PersistEntityBackend val ~ PersistMonadBackend m)
                => [Filter val] -> m ()

    -- | Get all records matching the given criterion in the specified order.
    -- Returns also the identifiers.
    selectSource
           :: (PersistEntity val, PersistEntityBackend val ~ PersistMonadBackend m)
           => [Filter val]
           -> [SelectOpt val]
           -> C.Source m (Entity val)

    -- | get just the first record for the criterion
    selectFirst :: (PersistEntity val, PersistEntityBackend val ~ PersistMonadBackend m)
                => [Filter val]
                -> [SelectOpt val]
                -> m (Maybe (Entity val))
    selectFirst filts opts = selectSource filts ((LimitTo 1):opts) C.$$ CL.head


    -- | Get the 'Key's of all records matching the given criterion.
    selectKeys :: (PersistEntity val, PersistEntityBackend val ~ PersistMonadBackend m)
               => [Filter val]
               -> [SelectOpt val]
               -> C.Source m (Key' val)

    -- | The total number of records fulfilling the given criterion.
    count :: (PersistEntity val, PersistEntityBackend val ~ PersistMonadBackend m)
          => [Filter val] -> m Int

#define DEF(T) { update k = lift . update k; updateGet k = lift . updateGet k; updateWhere f = lift . updateWhere f; deleteWhere = lift . deleteWhere; selectSource f = C.transPipe lift . selectSource f; selectFirst f = lift . selectFirst f; selectKeys f = C.transPipe lift . selectKeys f; count = lift . count }
#define GO(T) instance (PersistQuery m) => PersistQuery (T m) where DEF(T)
#define GOX(X, T) instance (X, PersistQuery m) => PersistQuery (T m) where DEF(T)

GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(ContT r)
GO(StateT s)
GO(ResourceT)
GO(Pipe l i o u)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)

#undef DEF
#undef GO
#undef GOX

type family BackendSpecificFilter b v

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
    | BackendFilter (BackendSpecificFilter (PersistEntityBackend v) v)


data SelectOpt v = forall typ. Asc (EntityField v typ)
                 | forall typ. Desc (EntityField v typ)
                 | OffsetBy Int
                 | LimitTo Int

-- | Call 'selectSource' but return the result as a list.
selectList :: (PersistEntity val, PersistQuery m, PersistEntityBackend val ~ PersistMonadBackend m)
           => [Filter val]
           -> [SelectOpt val]
           -> m [Entity val]
selectList a b = selectSource a b C.$$ CL.consume

-- | Call 'selectKeys' but return the result as a list.
selectKeysList :: (PersistEntity val, PersistQuery m, PersistEntityBackend val ~ PersistMonadBackend m)
               => [Filter val]
               -> [SelectOpt val]
               -> m [Key' val]
selectKeysList a b = selectKeys a b C.$$ CL.consume

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

deleteCascadeWhere :: (DeleteCascade a, PersistQuery m, PersistEntityBackend a ~ PersistMonadBackend m)
                   => [Filter a] -> m ()
deleteCascadeWhere filts = selectKeys filts [] C.$$ CL.mapM_ deleteCascade
