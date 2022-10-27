{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeGenTest (query0, spec) where

import Database.Persist.Sql
import Test.Hspec
import Database.Persist.Sql.Raw.QQ
import PersistentTestModels
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Resource
import Data.Text (Text)
import Control.Monad.Reader

spec :: (forall a. SqlPersistT (LoggingT (ResourceT IO)) a -> IO a) -> Spec
spec db = describe "CodeGenTest" $ do
    it "works" $ do
        _ <- db $ mapReaderT liftIO query0
        pure ()

query0 :: SqlPersistT IO [(Single Text, Single Int, Single (Maybe Text))]
query0 = --
    [sqlQQ|
        select
            ^{Person}.@{PersonName}, ^{Person}.@{PersonAge}, ^{Person}.@{PersonColor}
        from ^{Person}
        where @{PersonAge} =
            #{int} +
            #{int} +
            #{int} +
            #{int} + -- comments work ok
            #{int} +
            #{int} +
                0
    |]
  where
    int = 1 :: Int
