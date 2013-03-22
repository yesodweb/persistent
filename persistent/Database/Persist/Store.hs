{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
-- This is to test our assumption that OverlappingInstances is just for String
#ifndef NO_OVERLAP
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | API for database actions. The API deals with fields and entities.
-- In SQL, a field corresponds to a column, and should be a single non-composite value.
-- An entity corresponds to a SQL table, so an entity is a collection of fields.
module Database.Persist.Store
    ( PersistValue (..)
    , SqlType (..)
    , PersistField (..)
    , PersistEntity (..)
    , PersistStore (..)
    , PersistUnique (..)
    , PersistFilter (..)
    , SomePersistField (..)

    , ZT (..) -- ZonedTime wrapper

    , insertBy
    , getByValue
    , getJust
    , belongsTo
    , belongsToJust

    , DeleteCascade (..)
    , PersistException (..)
    , KeyBackend (..)
    , Key
    , Entity (..)

      -- * Helpers
    , listToJSON
    , mapToJSON

      -- * Config
    , PersistConfig (..)
    ) where

import Database.Persist.Types
import Database.Persist.Class.PersistStore
import Database.Persist.Class.PersistUnique
import Database.Persist.Class.PersistField
import Database.Persist.Class.PersistEntity
import Database.Persist.Class.PersistConfig
import Database.Persist.Class.DeleteCascade

import qualified Prelude
import Prelude hiding ((++), show)
import Data.Monoid (mappend)
import Data.Time (Day, TimeOfDay, UTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC, zonedTimeToLocalTime, zonedTimeZone)
import Data.ByteString.Char8 (ByteString, unpack)
import Control.Applicative
import Data.Typeable (Typeable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)

import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as L

import qualified Control.Exception as E
import Control.Monad.Trans.Error (Error (..))
import Database.Persist.EntityDef

import Data.Bits (bitSize)
import Control.Monad (liftM, (<=<))
import Control.Arrow (second)

import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Web.PathPieces (PathPiece (..))
import qualified Data.Text.Read

import Data.Aeson (Value)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Number as AN
import qualified Data.Vector as V

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as B64

import Data.Aeson (toJSON)
import Data.Aeson.Encode (fromValue)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
  (liftIO $ E.throwIO $ PersistForeignConstraintUnmet $ show key)
  return


infixr 5 ++
(++) :: T.Text -> T.Text -> T.Text
(++) = mappend

show :: Show a => a -> T.Text
show = T.pack . Prelude.show

listToJSON :: [PersistValue] -> T.Text
listToJSON = toStrict . toLazyText . fromValue . toJSON

mapToJSON :: [(T.Text, PersistValue)] -> T.Text
mapToJSON = toStrict . toLazyText . fromValue . toJSON
