{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist.Class.PersistStore
    ( PersistStore (..)
    ) where

import qualified Prelude
import Prelude hiding ((++), show)

import Control.Monad.Trans.Error (Error (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid (Monoid)

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

class MonadIO m => PersistStore m where
    type PersistMonadBackend m

    -- | Create a new record in the database, returning an automatically created
    -- key (in SQL an auto-increment id).
    insert :: (PersistMonadBackend m ~ PersistEntityBackend val, PersistEntity val)
           => val -> m (Key val)

    -- | Same as 'insert', but doesn't return a @Key@.
    insert_ :: (PersistMonadBackend m ~ PersistEntityBackend val, PersistEntity val)
            => val -> m ()
    insert_ val = insert val >> return ()

    -- | Create a new record in the database using the given key.
    insertKey :: (PersistMonadBackend m ~ PersistEntityBackend val, PersistEntity val)
              => Key val -> val -> m ()

    -- | Put the record in the database with the given key.
    -- Unlike 'replace', if a record with the given key does not
    -- exist then a new record will be inserted.
    repsert :: (PersistMonadBackend m ~ PersistEntityBackend val, PersistEntity val)
            => Key val -> val -> m ()

    -- | Replace the record in the database with the given
    -- key. Note that the result is undefined if such record does
    -- not exist, so you must use 'insertKey or 'repsert' in
    -- these cases.
    replace :: (PersistMonadBackend m ~ PersistEntityBackend val, PersistEntity val)
            => Key val -> val -> m ()

    -- | Delete a specific record by identifier. Does nothing if record does
    -- not exist.
    delete :: (PersistMonadBackend m ~ PersistEntityBackend val, PersistEntity val)
           => Key val -> m ()

    -- | Get a record by identifier, if available.
    get :: (PersistMonadBackend m ~ PersistEntityBackend val, PersistEntity val)
        => Key val -> m (Maybe val)

#define DEF(T) { type PersistMonadBackend (T m) = PersistMonadBackend m; insert = lift . insert; insertKey k = lift . insertKey k; repsert k = lift . repsert k; replace k = lift . replace k; delete = lift . delete; get = lift . get }
#define GO(T) instance (PersistStore m) => PersistStore (T m) where DEF(T)
#define GOX(X, T) instance (X, PersistStore m) => PersistStore (T m) where DEF(T)

GO(LoggingT)
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(ContT r)
GO(StateT s)
GO(ResourceT)
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
