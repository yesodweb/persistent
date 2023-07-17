{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.CompositeKeyPathMultiPieceSpec (spec) where

import Data.Text
import Database.Persist.Class.PersistEntity
import Database.Persist.TH
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Web.PathPieces

mkPersist sqlSettings {mpsDeriveInstances = [''PathMultiPiece]}
  [persistLowerCase|
    CompositeKeyEntity
      keyField1 Int
      keyField2 Int
      Primary keyField1 keyField2
  |]

spec :: Spec
spec = describe "CompositeKeyPathMultiPieceSpec" $ do
    describe "fromPathMultiPiece" $ do
        prop "orders fields correctly" $ \k1 k2 -> do
            let key :: Maybe (Key CompositeKeyEntity)
                key = fromPathMultiPiece (pack . show <$> [k1 :: Int, k2])
            compositeKeyEntityKeykeyField1 <$> key `shouldBe` Just k1
            compositeKeyEntityKeykeyField2 <$> key `shouldBe` Just k2
        let rejected :: Maybe (Key CompositeKeyEntity)
            rejected = Nothing
        prop "rejects paths with too many/few pieces" $ \n -> do
            let badPath = Prelude.replicate (abs n) (pack "0")
            abs n /= 2 ==> fromPathMultiPiece badPath `shouldBe` rejected
        it "rejects paths with pieces of incorrect types" $ do
            fromPathMultiPiece (pack <$> ["a", "0"]) `shouldBe` rejected
            fromPathMultiPiece (pack <$> ["0", "a"]) `shouldBe` rejected
    describe "toPathMultiPiece" $ do
        prop "orders fields correctly" $ \k1 k2 -> do
            let key = CompositeKeyEntityKey k1 k2
                path = toPathMultiPiece key
            path `shouldBe` pack . show <$> [k1, k2]
