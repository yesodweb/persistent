{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TransactionLevelTest where

import Init

share [mkPersist sqlSettings, mkMigrate "migration"] [persistUpperCase|
  Wombat
     name        Text sqltype=varchar(80)

     Primary name
     deriving Eq Show Ord

|]

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "IsolationLevel" $ do
  let item = Wombat "uno"
      isolationLevels = [minBound..maxBound]
  forM_ isolationLevels $ \il -> describe "insertOnDuplicateKeyUpdate" $ do
    it (show il ++ " works") $ runDb $ do
      transactionUndoWithIsolation il
      deleteWhere ([] :: [Filter Wombat])
      _ <- insert item
      Just item' <- get (WombatKey "uno")
      item' @== item
