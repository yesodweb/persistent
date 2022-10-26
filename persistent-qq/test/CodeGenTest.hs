{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -ddump-splices #-}

module CodeGenTest (query0, spec) where

import qualified Data.Text
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
   --  pure []

   -- [sqlQQ|
   --     select ^{Person}.@{PersonName}, ^{Person}.@{PersonAge}, ^{Person}.@{PersonColor}
   --     from ^{Person}
   --     where @{PersonName} = 'asdf'
   -- |]

--    [sqlQQ|
--        select @{PersonName}, @{PersonAge}, @{PersonColor}
--        from ^{Person}
--        where @{PersonName} = 'asdf
--    |]

    [sqlQQ|
        select
            ^{Person}.name, ^{Person}.age, ^{Person}.@{PersonColor}
             , ^{Person}.name, ^{Person}.age, ^{Person}.@{PersonColor}
             , ^{Person}.name, ^{Person}.age, ^{Person}.@{PersonColor}
        from ^{Person}
        where age =
            #{int} +
            #{int} +
            #{int} +
            #{int} +
            #{int} +
            #{int} +
                0
    |]
--   ((uncurry rawSql )
--         =<<
--           fmap ( \(txt, vals) ->
--
--                          ((Data.Text.pack $ concat [
--                     "select name, age, color from person where age = " <>
--                              "?" <> " + " <> "?" <> txt <> "" <> "" <> "" <> "asdf asdf asdf asdf asdf asdf asdf asdf " <> "asdfasdfasdfasdfasdfadsfasdfadsf some really large huge extremely large text value woWOWWWW it's so big"]), concat ([toPersistValue int] : [toPersistValue int] : vals))
--               )
----             (
----                (
----                   (
----                      ((fmap
----                          $ Database.Persist.Sql.Raw.QQ.first
----                              (Data.Text.pack " + " <>))
----                         ((fmap
----                             (\ (str_afnS, vals_afnT)
----                                -> (("?" <> str_afnS), ([toPersistValue int] : vals_afnT))))
----                            ((fmap
----                                $ Database.Persist.Sql.Raw.QQ.first
----                                    (Data.Text.pack " + 0 " <>))
--                               (return ("", [])))
--                               --))))))

-- query0 :: MonadIO m => SqlPersistT m [(Single Text, Single Int, Single (Maybe Text))]
-- query0 = [sqlQQ|
--     select name, age, color
--     from ^{Person}
--     where name = 'asdf'
--         |]
  where
    int = 1 :: Int
