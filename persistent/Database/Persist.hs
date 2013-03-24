{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist
    ( PersistField (..)
    , PersistEntity (..)
    , PersistStore (..)
    , PersistUnique (..)
    , PersistQuery (..)
    , KeyBackend (..)
    , Key
    , Entity (..)
    , insertBy
    , getJust
    , belongsTo
    , belongsToJust
    , getByValue
    , selectList
    , deleteCascadeWhere

    , SelectOpt (..)
    , Filter (..)

    -- * query combinators
    , (=.), (+=.), (-=.), (*=.), (/=.)
    , (==.), (!=.), (<.), (>.), (<=.), (>=.)
    , (<-.), (/<-.)
    , (||.)
    ) where

import Database.Persist.Query
import Database.Persist.Types
import Database.Persist.Class
import qualified Data.Text as T
import qualified Control.Exception as E
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)

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

-- | curry this to make a convenience function that loads an associated model
--   > foreign = belongsTo foeignId
belongsTo ::
  (PersistStore m
  , PersistEntity ent1
  , PersistEntity ent2
  , PersistMonadBackend m ~ PersistEntityBackend ent2
  ) => (ent1 -> Maybe (Key ent2)) -> ent1 -> m (Maybe ent2)
belongsTo foreignKeyField model = case foreignKeyField model of
    Nothing -> return Nothing
    Just f -> get f

-- | same as belongsTo, but uses @getJust@ and therefore is similarly unsafe
belongsToJust ::
  (PersistStore m
  , PersistEntity ent1
  , PersistEntity ent2
  , PersistMonadBackend m ~ PersistEntityBackend ent2)
  => (ent1 -> Key ent2) -> ent1 -> m ent2
belongsToJust getForeignKey model = getJust $ getForeignKey model

-- | Same as get, but for a non-null (not Maybe) foreign key
--   Unsafe unless your database is enforcing that the foreign key is valid
getJust :: (PersistStore m, PersistEntity val, Show (Key val), PersistMonadBackend m ~ PersistEntityBackend val) => Key val -> m val
getJust key = get key >>= maybe
  (liftIO $ E.throwIO $ PersistForeignConstraintUnmet $ T.pack $ show key)
  return
