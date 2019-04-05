{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module LargeNumberTest where

import Init
import Data.Word

#ifdef WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings,  mkMigrate "numberMigrate"] [persistLowerCase|
#endif
  Number
    intx Int
    int32 Int32
    word32 Word32
    int64 Int64
    word64 Word64
    deriving Show Eq
|]

#ifdef WITH_NOSQL
cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Number ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Number])
db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = specsWith db Number

specsWith
    ::
    ( PersistRecordBackend entity backend
    , PersistStoreWrite backend
    , Show entity, Eq entity
    , MonadIO m
    )
    => RunDb backend m
    -> (Int -> Int32 -> Word32 -> Int64 -> Word64 -> entity)
    -> Spec
specsWith runDb mkNumber = describe "Large Numbers" $ do
  it "preserves their values in the database" $ runDb $ do
      let go x = do
              xid <- insert x
              x' <- get xid
              liftIO $ x' @?= Just x

      go $ mkNumber maxBound 0 0 0 0
      go $ mkNumber 0 maxBound 0 0 0
      go $ mkNumber 0 0 maxBound 0 0
      go $ mkNumber 0 0 0 maxBound 0
      go $ mkNumber 0 0 0 0 maxBound

      go $ mkNumber minBound 0 0 0 0
      go $ mkNumber 0 minBound 0 0 0
      go $ mkNumber 0 0 minBound 0 0
      go $ mkNumber 0 0 0 minBound 0
      go $ mkNumber 0 0 0 0 minBound

