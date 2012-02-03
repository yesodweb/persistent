{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module LargeNumberTest where

import Init
import Data.Word

#if WITH_MONGODB
mkPersist MkPersistSettings { mpsBackend = ConT ''Action } [persist|
#else
share [mkPersist sqlSettings,  mkMigrate "numberMigrate"] [persist|
#endif
  Number
    intx Int
    int32 Int32
    word32 Word32
    int64 Int64
    word64 Word64
|]

cleanDB :: PersistQuery b m => b m ()
cleanDB = do
  deleteWhere ([] :: [Filter Number])

specs :: Specs
specs = describe "persistent" $ do
  it "large numbers" $ db $ do
      let go x = do
              xid <- insert x
              x' <- get xid
              liftIO $ x' @?= Just x

      go $ Number maxBound 0 0 0 0
      go $ Number 0 maxBound 0 0 0
      go $ Number 0 0 maxBound 0 0
      go $ Number 0 0 0 maxBound 0
      go $ Number 0 0 0 0 maxBound

      go $ Number minBound 0 0 0 0
      go $ Number 0 minBound 0 0 0
      go $ Number 0 0 minBound 0 0
      go $ Number 0 0 0 minBound 0
      go $ Number 0 0 0 0 minBound
