{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TypeLitFieldDefsTest (specsWith, typeLitFieldDefsMigrate) where

import Data.Maybe (fromJust)
import GHC.TypeLits
import Init

newtype Finite (n :: Nat) = Finite Int
    deriving (Show, Eq)

instance PersistField (Finite n) where
    toPersistValue (Finite n) = toPersistValue n
    fromPersistValue = fmap Finite . fromPersistValue

instance PersistFieldSql (Finite n) where
    sqlType _ = sqlType (Proxy :: Proxy Int)

newtype Labelled (t :: Symbol) = Labelled Int
    deriving (Show, Eq)

instance PersistField (Labelled n) where
    toPersistValue (Labelled n) = toPersistValue n
    fromPersistValue = fmap Labelled . fromPersistValue

instance PersistFieldSql (Labelled n) where
    sqlType _ = sqlType (Proxy :: Proxy Int)

share [mkPersist sqlSettings { mpsGeneric = True },  mkMigrate "typeLitFieldDefsMigrate"] [persistLowerCase|
    TypeLitFieldDefsNumeric
        one    (Finite 1)
        twenty (Finite 20)
        deriving Eq Show

    TypeLitFieldDefsLabelled
        one    (Labelled "one")
        twenty (Labelled "twenty")
        deriving Eq Show
|]

one :: Finite 1
one = Finite 1

oneLabelled :: Labelled "one"
oneLabelled = Labelled 1

twenty :: Finite 20
twenty = Finite 20

twentyLabelled :: Labelled "twenty"
twentyLabelled = Labelled 20

specsWith :: Runner backend m => RunDb backend m -> Spec
specsWith runDb =
    describe "Type Lit Field Definitions" $ do
        it "runs appropriate migrations" $ runDb $ do
            numKey <- insert $ TypeLitFieldDefsNumeric one twenty
            num <- getJust numKey
            liftIO $ typeLitFieldDefsNumericOne num @?= one
            liftIO $ typeLitFieldDefsNumericTwenty num @?= twenty

            labelledKey <- insert $ TypeLitFieldDefsLabelled oneLabelled twentyLabelled
            lbl <- getJust labelledKey
            liftIO $ typeLitFieldDefsLabelledOne lbl @?= oneLabelled
            liftIO $ typeLitFieldDefsLabelledTwenty lbl @?= twentyLabelled
