{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This will hopefully be the only module with CPP in it.
module Init (
  (@/=), (@==), (==@)
  , asIO
  , assertNotEqual
  , assertNotEmpty
  , assertEmpty
  , isTravis

  , module Database.Persist.Sql
  , persistSettings
  , MkPersistSettings (..)
  , BackendKey(..)
  , GenerateKey(..)

  , RunDb
  , Runner
   -- re-exports
  , module Database.Persist
  , module Test.Hspec
  , module Test.HUnit
  , mkPersist, mkMigrate, share, sqlSettings, persistLowerCase, persistUpperCase
  , Int32, Int64
  , Text
  , module Control.Monad.Reader
  , module Control.Monad
  , module Control.Monad.IO.Unlift
  , BS.ByteString
  , SomeException
  , MonadFail
  , TestFn(..)
  , truncateTimeOfDay
  , truncateToMicro
  , truncateUTCTime
  , arbText
  , liftA2
  , changeBackend
  , Proxy(..)
  , UUID(..)
  , sqlSettingsUuid
  ) where

#if !MIN_VERSION_monad_logger(0,3,30)
-- Needed for GHC versions 7.10.3. Can drop when we drop support for GHC
-- 7.10.3
import qualified Control.Monad.Fail as MonadFail
import Control.Monad.IO.Class
import Control.Monad.Logger
#endif

-- needed for backwards compatibility
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Resource.Internal

-- re-exports
import Control.Applicative (liftA2, (<|>))
import Control.Exception (SomeException)
import Control.Monad (forM_, liftM, replicateM, void, when)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader
import Data.Char (GeneralCategory(..), generalCategory)
import Data.Fixed (Micro, Pico)
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Test.Hspec
import Test.QuickCheck.Instances ()

import Data.Aeson (FromJSON, ToJSON, Value(..))
import qualified Data.Text.Encoding as TE
import Database.Persist.ImplicitIdDef (mkImplicitIdDef)
import Database.Persist.TH
       ( MkPersistSettings(..)
       , mkMigrate
       , mkPersist
       , persistLowerCase
       , persistUpperCase
       , setImplicitIdDef
       , share
       , sqlSettings
       )
import Web.Internal.HttpApiData
import Web.PathPieces

-- testing
import Test.HUnit (Assertion, assertBool, assertFailure, (@=?), (@?=))
import Test.QuickCheck

import Control.Monad (unless, (>=>))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Text (Text, unpack)
import System.Environment (getEnvironment)
import System.IO.Unsafe

import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH ()

-- Data types
import Data.Int (Int32, Int64)


asIO :: IO a -> IO a
asIO a = a

(@/=), (@==), (==@) :: (HasCallStack, Eq a, Show a, MonadIO m) => a -> a -> m ()
infix 1 @/= --, /=@
actual @/= expected = liftIO $ assertNotEqual "" expected actual

infix 1 @==, ==@
actual @== expected = liftIO $ actual @?= expected
expected ==@ actual = liftIO $ expected @=? actual

{-
expected /=@ actual = liftIO $ assertNotEqual "" expected actual
-}


assertNotEqual :: (Eq a, Show a, HasCallStack) => String -> a -> a -> Assertion
assertNotEqual preface expected actual =
  unless (actual /= expected) (assertFailure msg)
  where msg = (if null preface then "" else preface ++ "\n") ++
             "expected: " ++ show expected ++ "\n to not equal: " ++ show actual

assertEmpty :: (MonadIO m) => [a] -> m ()
assertEmpty xs = liftIO $ assertBool "" (null xs)

assertNotEmpty :: (MonadIO m) => [a] -> m ()
assertNotEmpty xs = liftIO $ assertBool "" (not (null xs))

isTravis :: IO Bool
isTravis = isCI

isCI :: IO Bool
isCI =  do
    env <- liftIO getEnvironment
    return $ case lookup "TRAVIS" env <|> lookup "CI" env of
        Just "true" -> True
        _ -> False


persistSettings :: MkPersistSettings
persistSettings = sqlSettings { mpsGeneric = True }

instance Arbitrary PersistValue where
    arbitrary = PersistInt64 `fmap` choose (0, maxBound)

instance PersistStore backend => Arbitrary (BackendKey backend) where
  arbitrary = (errorLeft . fromPersistValue) `fmap` arbitrary
    where
      errorLeft x = case x of
          Left e -> error $ unpack e
          Right r -> r

class GenerateKey backend where
    generateKey :: IO (BackendKey backend)

instance GenerateKey SqlBackend where
    generateKey = do
        i <- readIORef keyCounter
        writeIORef keyCounter (i + 1)
        return $ SqlBackendKey $ i

keyCounter :: IORef Int64
keyCounter = unsafePerformIO $ newIORef 1
{-# NOINLINE keyCounter #-}

-- | A datatype that wraps a function on @entity@ that can has testable results.
--
-- Allows us to write:
--
-- @
-- foo :: entity -> entity -> [TestFn entity] -> Bool
-- foo e0 e1 = all (\(TestFn msg f) -> f e0 == f e1)
-- @
data TestFn entity where
    TestFn
        :: (Show a, Eq a)
        => String
        -> (entity -> a)
        -> TestFn entity

truncateTimeOfDay :: TimeOfDay -> Gen TimeOfDay
truncateTimeOfDay (TimeOfDay h m s) =
  return $ TimeOfDay h m $ truncateToMicro s

-- truncate less significant digits
truncateToMicro :: Pico -> Pico
truncateToMicro p = let
  p' = fromRational . toRational $ p  :: Micro
  in   fromRational . toRational $ p' :: Pico

truncateUTCTime :: UTCTime -> Gen UTCTime
truncateUTCTime (UTCTime d dift) = do
  let pico = fromRational . toRational $ dift :: Pico
      picoi= truncate . (*1000000000000) . toRational $ truncateToMicro pico :: Integer
      -- https://github.com/lpsmith/postgresql-simple/issues/123
      d' = max d $ fromGregorian 1950 1 1
  return $ UTCTime d' $ picosecondsToDiffTime picoi

arbText :: Gen Text
arbText =
     T.pack
  .  filter ((`notElem` forbidden) . generalCategory)
  .  filter (<= '\xFFFF') -- only BMP
  .  filter (/= '\0')     -- no nulls
  .  T.unpack
  <$> arbitrary
  where forbidden = [NotAssigned, PrivateUse]

type Runner backend m =
    ( MonadIO m, MonadUnliftIO m, MonadFail m
    , MonadThrow m, MonadBaseControl IO m
    , PersistStoreWrite backend, PersistStoreWrite (BaseBackend backend)
    , GenerateKey backend
    , HasPersistBackend backend
    , PersistUniqueWrite backend
    , PersistQueryWrite backend
    , backend ~ BaseBackend backend
    , PersistQueryRead backend
    )

type RunDb backend m = ReaderT backend m () -> IO ()

changeBackend
    :: forall backend backend' m. MonadUnliftIO m
    => (backend -> backend')
    -> RunDb backend m
    -> RunDb backend' m
changeBackend f runDb =
    runDb . ReaderT . (. f) . runReaderT

#if !MIN_VERSION_monad_logger(0,3,30)
-- Needed for GHC versions 7.10.3. Can drop when we drop support for GHC
-- 7.10.3
instance MonadFail (LoggingT (ResourceT IO)) where
    fail = liftIO . MonadFail.fail
#endif

#if MIN_VERSION_resourcet(1,2,0)
-- This instance is necessary because LTS-9 and below are on
-- MonadBaseControl, while LTS 11 and up are on UnliftIO. We can drop this
-- instance (and related CPP) when we drop support for LTS-9 (GHC 8.0).
instance MonadBase b m => MonadBase b (ResourceT m) where
    liftBase = lift . liftBase
instance MonadBaseControl b m => MonadBaseControl b (ResourceT m) where
     type StM (ResourceT m) a = StM m a
     liftBaseWith f = ResourceT $ \reader' ->
         liftBaseWith $ \runInBase ->
             f $ runInBase . (\(ResourceT r) -> r reader')
     restoreM = ResourceT . const . restoreM
#endif

-- * For implicit ID spec

newtype UUID = UUID { unUUID :: Text }
    deriving stock
        (Show, Eq, Ord, Read)
    deriving newtype
        (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, PathPiece)

instance PersistFieldSql UUID where
    sqlType _ = SqlOther "UUID"

instance PersistField UUID where
    toPersistValue (UUID txt) =
        PersistLiteral_ Escaped (TE.encodeUtf8 txt)
    fromPersistValue pv =
        case pv of
            PersistLiteral_ Escaped bs ->
                Right $ UUID (TE.decodeUtf8 bs)
            _ ->
                Left "Nope"

sqlSettingsUuid :: Text -> MkPersistSettings
sqlSettingsUuid defExpr =
    let
        uuidDef =
           mkImplicitIdDef @UUID defExpr
        settings =
            setImplicitIdDef uuidDef sqlSettings
     in
        settings
