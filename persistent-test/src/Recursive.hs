{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Recursive (specsWith, recursiveMigrate, cleanup) where

import Init

share [mkPersist sqlSettings, mkMigrate "recursiveMigrate"] [persistLowerCase|

SubType
  object [MenuObject]
  deriving Show Eq

MenuObject
  sub SubType Maybe
  deriving Show Eq

|]

cleanup :: SqlPersistT IO ()
cleanup = do
  deleteWhere ([] :: [Filter MenuObject])
  deleteWhere ([] :: [Filter SubType])

specsWith
    ::
    ( MonadIO m
    )
    => RunDb SqlBackend m
    -> Spec
specsWith runDb = describe "recursive definitions" $ do
  it "mutually recursive" $ runDb $ do
    let m1 = MenuObject $ Just $ SubType []
    let m2 = MenuObject $ Just $ SubType [m1]
    let m3 = MenuObject $ Just $ SubType [m2]
    k3 <- insert m3
    m3' <- get k3
    m3' @== Just m3
