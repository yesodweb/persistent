{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Database.Persist.Class.PersistStore
    ( PersistStore (..)
    , getJust
    , belongsTo
    , belongsToJust
    ) where

import qualified Prelude
import Prelude hiding ((++), show)

import qualified Data.Text as T

import Control.Monad.Trans.Error (Error (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid (Monoid)
import Control.Exception.Lifted (throwIO)

import Data.Conduit.Internal (Pipe, ConduitM)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT   )
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import Control.Monad.Trans.Resource ( ResourceT)

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

import Database.Persist.Class.PersistEntity
import Database.Persist.Types

class MonadIO m => PersistStore m where
    type MonadBackend m

    -- | Get a record by identifier, if available.
    get :: (Show (Key record), MonadBackend m ~ EntityBackend record, PersistEntity record)
        => Key record -> m (Maybe record)

    -- | Create a new record in the database, returning an automatically created
    -- key (in SQL an auto-increment id).
    insert :: (MonadBackend m ~ EntityBackend record, PersistEntity record)
           => record -> m (Key record)

    -- | Same as 'insert', but doesn't return a key.
    insert_ :: (MonadBackend m ~ EntityBackend record, PersistEntity record)
            => record -> m ()
    insert_ val = insert val >> return ()

    -- | Create multiple records in the database.
    -- SQL backends currently use the slow default implementation of
    -- @mapM insert@
    insertMany :: (MonadBackend m ~ EntityBackend record, PersistEntity record)
                => [record] -> m [Key record]
    insertMany = mapM insert

    -- | Create a new record in the database using the given key.
    insertKey :: (MonadBackend m ~ EntityBackend record, PersistEntity record)
              => Key record -> record -> m ()

    -- | Put the record in the database with the given key.
    -- Unlike 'replace', if a record with the given key does not
    -- exist then a new record will be inserted.
    repsert :: (MonadBackend m ~ EntityBackend record, PersistEntity record, Show (Key record))
            => Key record -> record -> m ()

    -- | Replace the record in the database with the given
    -- key. Note that the result is undefined if such record does
    -- not exist, so you must use 'insertKey or 'repsert' in
    -- these cases.
    replace :: (MonadBackend m ~ EntityBackend record, PersistEntity record)
            => Key record -> record -> m ()

    -- | Delete a specific record by identifier. Does nothing if record does
    -- not exist.
    delete :: (MonadBackend m ~ EntityBackend record, PersistEntity record)
           => Key record -> m ()

-- | Same as get, but for a non-null (not Maybe) foreign key
--   Unsafe unless your database is enforcing that the foreign key is valid
getJust :: (PersistStore m, PersistEntity record, Show (Key record), MonadBackend m ~ EntityBackend record) => Key record -> m record
getJust key = get key >>= maybe
  (liftIO $ throwIO $ KeyNotFound $ Prelude.show key)
  return

-- | curry this to make a convenience function that loads an associated model
--   > foreign = belongsTo foreignId
belongsTo ::
  (PersistStore m
  , PersistEntity ent1
  , PersistEntity ent2
  , MonadBackend m ~ EntityBackend ent2
  , Show (Key ent2)
  ) => (ent1 -> Maybe (Key ent2)) -> ent1 -> m (Maybe ent2)
belongsTo foreignKeyField model = case foreignKeyField model of
    Nothing -> return Nothing
    Just f -> get f

-- | same as belongsTo, but uses @getJust@ and therefore is similarly unsafe
belongsToJust ::
  (PersistStore m
  , PersistEntity ent1
  , PersistEntity ent2
  , MonadBackend m ~ EntityBackend ent2
  , Show (Key ent2))
  => (ent1 -> Key ent2) -> ent1 -> m ent2
belongsToJust getForeignKey model = getJust $ getForeignKey model

#define DEF(T) { type MonadBackend (T m) = MonadBackend m; insert = lift . insert; insertKey k = lift . insertKey k; repsert k = lift . repsert k; replace k = lift . replace k; delete = lift . delete; get = lift . get }
#define GO(T)     instance (PersistStore m)    => PersistStore (T m) where DEF(T)
#define GOX(X, T) instance (X, PersistStore m) => PersistStore (T m) where DEF(T)

GO(LoggingT)
GO(ResourceT)
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(ContT r)
GO(StateT s)
GO(Pipe l i o u)
GO(ConduitM i o)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)

#undef DEF
#undef GO
#undef GOX
