{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module EquivalentTypeTestPostgres (specs) where

import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T

import Database.Persist.TH
import PgInit

share [mkPersist sqlSettings, mkMigrate "migrateAll1"] [persistLowerCase|
EquivalentType sql=equivalent_types
    field1 Int
    field2 T.Text sqltype=text
    field3 T.Text sqltype=us_postal_code
    deriving Eq Show
|]

share [mkPersist sqlSettings, mkMigrate "migrateAll2"] [persistLowerCase|
EquivalentType2 sql=equivalent_types
    field1 Int
    field2 T.Text
    field3 T.Text sqltype=us_postal_code
    deriving Eq Show
|]

specs :: Spec
specs = describe "doesn't migrate equivalent types" $ do
    it "works" $ asIO $ runResourceT $ runConn $ do

        _ <- rawExecute "DROP DOMAIN IF EXISTS us_postal_code CASCADE" []
        _ <- rawExecute "CREATE DOMAIN us_postal_code AS TEXT CHECK(VALUE ~ '^\\d{5}$')" []

        _ <- runMigrationSilent migrateAll1
        xs <- getMigration migrateAll2
        liftIO $ xs @?= []
