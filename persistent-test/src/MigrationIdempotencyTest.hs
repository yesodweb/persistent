{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MigrationIdempotencyTest where

import Database.Persist.TH
import qualified Data.Text as T

import Init

#ifdef WITH_NOSQL
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings, mkMigrate "migration"] [persistLowerCase|
#endif
Idempotency
    field1 Int
    field2 T.Text sqltype=varchar(5)
    field3 T.Text sqltype=mediumtext
    field4 T.Text sqltype=longtext
    field5 T.Text sqltype=mediumblob
    field6 T.Text sqltype=longblob
    field7 Double sqltype=double(6,5)
|]


specs :: Spec
specs = describe "MySQL migration with backend-specific sqltypes" $ do
#ifdef WITH_NOSQL
    return ()
#else
  it "is idempotent" $ db $ do
      again <- getMigration migration
      liftIO $ again @?= []
#endif
