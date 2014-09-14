{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module LargeNumberTest where

import Init
import Data.Word

#ifdef WITH_MONGODB
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

#ifdef WITH_MONGODB
cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Number ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Number])
db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
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
