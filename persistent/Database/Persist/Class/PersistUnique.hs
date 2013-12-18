{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Database.Persist.Class.PersistUnique
    ( PersistUnique (..)
    , getByValue
    , insertBy
    , replaceUnique
    , checkUnique
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
-- SQL backends automatically create uniqueness constraints, but for MongoDB you must manually place a unique index on the field.
--
-- Some functions in this module (insertUnique, insertBy, and replaceUnique) first query the unique indexes to check for conflicts.
-- You could instead optimistically attempt to perform the operation (e.g. replace instead of replaceUnique). However,
--
--  * there is some fragility to trying to catch the correct exception and determing the column of failure.
--
--  * an exception will automatically abort the current SQL transaction
class PersistStore m => PersistUnique m where
    -- | Get a record by unique key, if available. Returns also the identifier.
    getBy :: (EntityBackend val ~ MonadBackend m, PersistEntity val) => Unique val -> m (Maybe (Entity val))

    -- | Delete a specific record by unique key. Does nothing if no record
    -- matches.
    deleteBy :: (EntityBackend val ~ MonadBackend m, PersistEntity val) => Unique val -> m ()

    -- | Like 'insert', but returns 'Nothing' when the record
    -- couldn't be inserted because of a uniqueness constraint.
    insertUnique :: (EntityBackend val ~ MonadBackend m, PersistEntity val) => val -> m (Maybe (Key val))
    insertUnique datum = do
        conflict <- checkUnique datum
        case conflict of
          Nothing -> Just `liftM` insert datum
          Just _ -> return Nothing

-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key is returned as 'Right'.
insertBy :: (PersistEntity val, PersistUnique m, EntityBackend val ~ MonadBackend m)
         => val -> m (Either (Entity val) (Key val))

insertBy val = do
    res <- getByValue val
    case res of
      Nothing -> Right `liftM` insert val
      Just z -> return $ Left z

-- | A modification of 'getBy', which takes the 'PersistEntity' itself instead
-- of a 'Unique' value. Returns a value matching /one/ of the unique keys. This
-- function makes the most sense on entities with a single 'Unique'
-- constructor.
getByValue :: (PersistEntity value, PersistUnique m, EntityBackend value ~ MonadBackend m)
           => value -> m (Maybe (Entity value))
getByValue = checkUniques . persistUniqueKeys
  where
    checkUniques [] = return Nothing
    checkUniques (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> checkUniques xs
            Just z -> return $ Just z


-- | Attempt to replace the record of the given key with the given new record.
-- First query the unique fields to make sure the replacement maintains uniqueness constraints.
-- Return 'Nothing' if the replacement was made.
-- If uniqueness is violated, return a 'Just' with the 'Unique' violation
--
-- Since 1.2.2.0
replaceUnique :: (Eq record, Eq (Unique record), EntityBackend record ~ MonadBackend m, PersistEntity record, PersistStore m, PersistUnique m, Show (Key record))
              => Key record -> record -> m (Maybe (Unique record))
replaceUnique key datumNew = getJust key >>= replaceOriginal
  where
    uniqueKeysNew = persistUniqueKeys datumNew
    replaceOriginal original = do
        conflict <- checkUniqueKeys changedKeys
        case conflict of
          Nothing -> replace key datumNew >> return Nothing
          (Just conflictingKey) -> return $ Just conflictingKey
      where
        changedKeys = uniqueKeysNew \\ uniqueKeysOriginal
        uniqueKeysOriginal = persistUniqueKeys original

-- | Check whether there are any conflicts for unique keys with this entity and
-- existing entities in the database.
--
-- Returns 'Nothing' if the entity would be unique, and could thus safely be inserted.
-- on a conflict returns the conflicting key
checkUnique :: (EntityBackend record ~ MonadBackend m, PersistEntity record, PersistUnique m)
            => record -> m (Maybe (Unique record))
checkUnique = checkUniqueKeys . persistUniqueKeys

checkUniqueKeys :: (PersistEntity record, PersistUnique m, EntityBackend record ~ MonadBackend m)
                => [Unique record] -> m (Maybe (Unique record))
checkUniqueKeys [] = return Nothing
checkUniqueKeys (x:xs) = do
    y <- getBy x
    case y of
        Nothing -> checkUniqueKeys xs
        Just _ -> return (Just x)

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
