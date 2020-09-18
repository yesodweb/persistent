{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.Logger (LoggingT, runLoggingT)
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (sort)
import Data.Text (Text)
import System.Log.FastLogger (fromLogStr)
import Test.Hspec
import Test.HUnit ((@?=))

import Database.Persist.Sql
import Database.Persist.Sql.Raw.QQ
import Database.Persist.Sqlite
import PersistTestPetType
import PersistentTestModels

main :: IO ()
main = hspec spec

_debugOn :: Bool
_debugOn = False

runConn :: MonadUnliftIO m => SqlPersistT (LoggingT m) a -> m a
runConn f = do
  let printDebug = when _debugOn . B.putStrLn . fromLogStr
  flip runLoggingT (\_ _ _ s -> printDebug s) $ do
    withSqliteConn ":memory:" $ runSqlConn f

db :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
db actions = do
  runResourceT $ runConn $ do
      _ <- runMigrationSilent testMigrate
      actions <* transactionUndo

spec :: Spec
spec = describe "persistent-qq" $ do
    it "sqlQQ/?-?" $ db $ do
        ret <- [sqlQQ| SELECT #{2 :: Int}+#{2 :: Int} |]
        liftIO $ ret @?= [Single (4::Int)]

    it "sqlQQ/?-?" $ db $ do
        ret <- [sqlQQ| SELECT #{5 :: Int}-#{3 :: Int} |]
        liftIO $ ret @?= [Single (2::Int)]

    it "sqlQQ/NULL" $ db $ do
        ret <- [sqlQQ| SELECT NULL |]
        liftIO $ ret @?= [Nothing :: Maybe (Single Int)]

    it "sqlQQ/entity" $ db $ do
        let insert'
              :: PersistStore backend
              => PersistEntity val
              => PersistEntityBackend val ~ BaseBackend backend
              => MonadIO m
              => val
              -> ReaderT backend m (Key val, val)
            insert' v = insert v >>= \k -> return (k, v)
        (p1k, p1) <- insert' $ Person "Mathias"   23 Nothing
        (p2k, p2) <- insert' $ Person "Norbert"   44 Nothing
        (p3k, _ ) <- insert' $ Person "Cassandra" 19 Nothing
        (_  , _ ) <- insert' $ Person "Thiago"    19 Nothing
        (a1k, a1) <- insert' $ Pet p1k "Rodolfo" Cat
        (a2k, a2) <- insert' $ Pet p1k "Zeno"    Cat
        (a3k, a3) <- insert' $ Pet p2k "Lhama"   Dog
        (_  , _ ) <- insert' $ Pet p3k "Abacate" Cat

        let runQuery
              :: (RawSql a, Functor m, MonadIO m)
              => Int
              -> ReaderT SqlBackend m [a]
            runQuery age =
              [sqlQQ|
                  SELECT ??, ??
                  FROM
                    ^{Person},
                    ^{Pet}
                  WHERE ^{Person}.@{PersonAge} >= #{age}
                      AND ^{Pet}.@{PetOwnerId} = ^{Person}.@{PersonId}
                      ORDER BY ^{Person}.@{PersonName}
              |]

        ret <- runQuery 20
        liftIO $ ret @?= [ (Entity p1k p1, Entity a1k a1)
                         , (Entity p1k p1, Entity a2k a2)
                         , (Entity p2k p2, Entity a3k a3) ]
        ret2 <- runQuery 20
        liftIO $ ret2 @?= [ (Just (Entity p1k p1), Just (Entity a1k a1))
                          , (Just (Entity p1k p1), Just (Entity a2k a2))
                          , (Just (Entity p2k p2), Just (Entity a3k a3)) ]
        ret3 <- runQuery 20
        liftIO $ ret3 @?= [ Just (Entity p1k p1, Entity a1k a1)
                          , Just (Entity p1k p1, Entity a2k a2)
                          , Just (Entity p2k p2, Entity a3k a3) ]

    it "sqlQQ/order-proof" $ db $ do
        let p1 = Person "Zacarias" 93 Nothing
        p1k <- insert p1

        let runQuery
              :: (RawSql a, Functor m, MonadIO m)
              => ReaderT SqlBackend m [a]
            runQuery = [sqlQQ| SELECT ?? FROM ^{Person} |]
        ret1 <- runQuery
        ret2 <- runQuery :: (MonadIO m) => SqlPersistT m [Entity (ReverseFieldOrder Person)]
        liftIO $ ret1 @?= [Entity p1k p1]
        liftIO $ ret2 @?= [Entity (RFOKey $ unPersonKey p1k) (RFO p1)]

    it "sqlQQ/OUTER JOIN" $ db $ do
        let insert' :: (PersistStore backend, PersistEntity val, PersistEntityBackend val ~ BaseBackend backend, MonadIO m)
                    => val -> ReaderT backend m (Key val, val)
            insert' v = insert v >>= \k -> return (k, v)
        (p1k, p1) <- insert' $ Person "Mathias"   23 Nothing
        (p2k, p2) <- insert' $ Person "Norbert"   44 Nothing
        (a1k, a1) <- insert' $ Pet p1k "Rodolfo" Cat
        (a2k, a2) <- insert' $ Pet p1k "Zeno"    Cat
        ret <- [sqlQQ|
          SELECT ??, ??
          FROM ^{Person}
          LEFT OUTER JOIN ^{Pet}
              ON ^{Person}.@{PersonId} = ^{Pet}.@{PetOwnerId}
          ORDER BY ^{Person}.@{PersonName}
        |]
        liftIO $ ret @?= [ (Entity p1k p1, Just (Entity a1k a1))
                         , (Entity p1k p1, Just (Entity a2k a2))
                         , (Entity p2k p2, Nothing) ]

    it "sqlQQ/values syntax" $ db $ do
        let insert' :: (PersistStore backend, PersistEntity val, PersistEntityBackend val ~ BaseBackend backend, MonadIO m)
                    => val -> ReaderT backend m (Key val, val)
            insert' v = insert v >>= \k -> return (k, v)
        (p1k, p1) <- insert' $ Person "Mathias"   23 (Just "red")
        (_  , _ ) <- insert' $ Person "Norbert"   44 (Just "green")
        (p3k, p3) <- insert' $ Person "Cassandra" 19 (Just "blue")
        (_  , _ ) <- insert' $ Person "Thiago"    19 (Just "yellow")
        let
          colors = Just "blue" :| Just "red" : [] :: NonEmpty (Maybe Text)
        ret <- [sqlQQ|
          SELECT ??
          FROM ^{Person}
          WHERE ^{Person}.@{PersonColor} IN %{colors}
        |]
        liftIO $ ret @?= [ (Entity p1k p1)
                         , (Entity p3k p3) ]

    it "sqlQQ/rows syntax" $ do
        let rows :: NonEmpty (Text, Int)
            rows = ("Mathias", 23) :| ("Norbert", 44) : ("Cassandra", 19) : []
        db $ do
            [executeQQ|INSERT INTO ^{Person} (@{PersonName}, @{PersonAge}) VALUES *{rows}|]
            map unSingle <$> [sqlQQ|SELECT @{PersonName} FROM ^{Person} ORDER BY @{PersonName}|]
        `shouldReturn` sort ["Mathias", "Norbert", "Cassandra" :: Text]
