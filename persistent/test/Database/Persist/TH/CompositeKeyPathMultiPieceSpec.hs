{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.CompositeKeyPathMultiPieceSpec (spec) where

import Data.Text
import Database.Persist.Class.PersistEntity
import Database.Persist.Class.PersistPathMultiPiece
import Database.Persist.TH
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Web.PathPieces

mkPersist sqlSettings
  [persistLowerCase|
    CompositeKeyedEntity
      keyField1 Int
      keyField2 Int
      Primary keyField1 keyField2
      deriving PathMultiPiece
  |]

spec :: Spec
spec = describe "CompositeKeyPathMultiPieceSpec" $ do
    describe "fromPathMultiPiece" $ do
        prop "orders fields correctly" $ \k1 k2 -> do
            let key :: Maybe (Key CompositeKeyedEntity)
                key = fromPathMultiPiece (pack . show <$> [k1 :: Int, k2])
            compositeKeyedEntityKeykeyField1 <$> key `shouldBe` Just k1
            compositeKeyedEntityKeykeyField2 <$> key `shouldBe` Just k2
        let rejected :: Maybe (Key CompositeKeyedEntity)
            rejected = Nothing
        prop "rejects paths with too many/few pieces" $ \n -> do
            let badPath = Prelude.replicate (abs n) (pack "0")
            abs n /= 2 ==> fromPathMultiPiece badPath `shouldBe` rejected
        it "rejects paths with pieces of incorrect types" $ do
            fromPathMultiPiece (pack <$> ["a", "0"]) `shouldBe` rejected
            fromPathMultiPiece (pack <$> ["0", "a"]) `shouldBe` rejected
    describe "toPathMultiPiece" $ do
        prop "orders fields correctly" $ \k1 k2 -> do
            let key = CompositeKeyedEntityKey k1 k2
                path = toPathMultiPiece key
            path `shouldBe` pack . show <$> [k1, k2]
