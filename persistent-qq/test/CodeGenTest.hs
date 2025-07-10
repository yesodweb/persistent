{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module CodeGenTest (query0, spec) where

import Control.Monad.Logger (LoggingT)
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.Sql.Raw.QQ
import PersistentTestModels
import Test.Hspec

spec :: (forall a. SqlPersistT (LoggingT (ResourceT IO)) a -> IO a) -> Spec
spec db = describe "CodeGenTest" $ do
    it "works" $ do
        _ <- db $ mapReaderT liftIO query0
        pure ()

query0 :: SqlPersistT IO [(Single Text, Single Int, Single (Maybe Text))]
query0 =
    --
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
