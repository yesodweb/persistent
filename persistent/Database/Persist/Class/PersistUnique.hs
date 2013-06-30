{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Database.Persist.Class.PersistUnique
    ( PersistUnique (..)
    , getByValue
    , insertBy
    , replaceUnique
    ) where

import qualified Prelude
import Prelude hiding ((++), show)

import Control.Monad (liftM)
import Control.Monad.Trans.Error (Error (..))
import Control.Monad.Trans.Class (lift)
import Data.Monoid (Monoid)
import Data.List ((\\))

import Data.Conduit.Internal (Pipe)
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
import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistEntity

-- | Queries against unique keys (other than the id).
--
-- Please read the general Persistent documentation to learn how to create
-- Unique keys.
-- SQL backends automatically create uniqueness constraints, but for MongoDB you must place a unique index on the field.
class PersistStore m => PersistUnique m where
    -- | Get a record by unique key, if available. Returns also the identifier.
    getBy :: (PersistEntityBackend val ~ PersistMonadBackend m, PersistEntity val) => Unique val -> m (Maybe (Entity val))

    -- | Delete a specific record by unique key. Does nothing if no record
    -- matches.
    deleteBy :: (PersistEntityBackend val ~ PersistMonadBackend m, PersistEntity val) => Unique val -> m ()

    -- | Like 'insert', but returns 'Nothing' when the record
    -- couldn't be inserted because of a uniqueness constraint.
    insertUnique :: (PersistEntityBackend val ~ PersistMonadBackend m, PersistEntity val) => val -> m (Maybe (Key val))
    insertUnique datum = do
        isUnique <- checkUnique datum
        if isUnique then Just `liftM` insert datum else return Nothing

-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key is returned as 'Right'.
insertBy :: (PersistEntity v, PersistStore m, PersistUnique m, PersistMonadBackend m ~ PersistEntityBackend v)
          => v -> m (Either (Entity v) (Key v))
insertBy val =
    go $ persistUniqueKeys val
  where
    go [] = Right `liftM` insert val
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just z -> return $ Left z

-- | A modification of 'getBy', which takes the 'PersistEntity' itself instead
-- of a 'Unique' value. Returns a value matching /one/ of the unique keys. This
-- function makes the most sense on entities with a single 'Unique'
-- constructor.
getByValue :: (PersistEntity v, PersistUnique m, PersistEntityBackend v ~ PersistMonadBackend m)
           => v -> m (Maybe (Entity v))
getByValue val =
    go $ persistUniqueKeys val
  where
    go [] = return Nothing
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just z -> return $ Just z

-- | attempt to replace the record of the given key with the given new record
-- First query the unique fields to make sure the replacement maintains uniqueness constraints
-- Return true if the replacement was made, false if it was not because it violates uniqueness.
replaceUnique :: (Eq record, Eq (Unique record), PersistEntityBackend record ~ PersistMonadBackend m, PersistEntity record, PersistStore m, PersistUnique m)
               => Key record -> record -> m Bool
replaceUnique key datumNew = getJust key >>= replaceOriginal
  where
    uniqueKeysNew = persistUniqueKeys datumNew
    replaceOriginal original = replaceByKeys changedKeys
      where
        changedKeys = uniqueKeysOriginal \\ uniqueKeysNew
        uniqueKeysOriginal = persistUniqueKeys original

    replaceByKeys [] = replace key datumNew >> return True
    replaceByKeys (key:keys) = do
      conflict <- getBy key
      case conflict of
           Nothing -> replaceByKeys keys
           (Just _) -> return False

#define DEF(T) { getBy = lift . getBy; deleteBy = lift . deleteBy; insertUnique = lift . insertUnique }
#define GO(T) instance (PersistUnique m) => PersistUnique (T m) where DEF(T)
#define GOX(X, T) instance (X, PersistUnique m) => PersistUnique (T m) where DEF(T)

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
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)

#undef DEF
#undef GO
#undef GOX

-- | Check whether there are any conflicts for unique keys with this entity and
-- existing entities in the database.
--
-- Returns 'True' if the entity would be unique, and could thus safely be
-- 'insert'ed; returns 'False' on a conflict.
checkUnique :: (PersistEntityBackend val ~ PersistMonadBackend m, PersistEntity val, PersistUnique m) => val -> m Bool
checkUnique val =
    go $ persistUniqueKeys val
  where
    go [] = return True
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just _ -> return False
