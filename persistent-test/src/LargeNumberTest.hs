{-# LANGUAGE UndecidableInstances #-}
module LargeNumberTest where

import Data.Word

import Init

share [mkPersist sqlSettings { mpsGeneric = True },  mkMigrate "numberMigrate"] [persistLowerCase|
  Number
    intx Int
    int32 Int32
    word32 Word32
    int64 Int64
    word64 Word64
    deriving Show Eq
|]

cleanDB
    :: Runner backend m => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter (NumberGeneric backend)])

specsWith :: Runner backend m => RunDb backend m -> Spec
specsWith runDb = describe "Large Numbers" $ do
  it "preserves their values in the database" $ runDb $ do
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

