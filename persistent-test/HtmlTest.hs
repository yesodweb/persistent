{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module HtmlTest (specs) where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit
import Database.Persist.Sqlite
import Database.Persist.TH
#if WITH_POSTGRESQL
import Database.Persist.Postgresql
#endif
import Data.Char (generalCategory, GeneralCategory(..))
import qualified Data.Text as T
import System.Random (randomIO, randomRIO, Random)
import Control.Applicative ((<$>))

import Init
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

-- Test lower case names
share [mkPersist sqlSettings, mkMigrate "htmlMigrate"] [persistLowerCase|
HtmlTable
    html Html
    deriving
|]

cleanDB :: PersistQuery b m => b m ()
cleanDB = do
  deleteWhere ([] :: [Filter HtmlTable])

specs :: Spec
specs = describe "html" $ do
    it "works" $ asIO $ runConn $ do
#ifndef WITH_MONGODB
        _ <- runMigrationSilent htmlMigrate
        -- Ensure reading the data from the database works...
        _ <- runMigrationSilent htmlMigrate
#endif

        sequence_ $ replicate 1000 $ do
            x <- liftIO randomValue
            key <- insert $ HtmlTable x
            Just (HtmlTable y) <- get key
            liftIO $ do
                renderHtml x @?= renderHtml y

randomValue :: IO Html
randomValue =
                preEscapedToMarkup
              . T.pack
              . filter ((`notElem` forbidden) . generalCategory)
              . filter (<= '\xFFFF') -- only BMP
              . filter (/= '\0')     -- no nulls
         <$> randomIOs
    where forbidden = [NotAssigned, PrivateUse]

asIO :: IO a -> IO a
asIO = id

randomIOs :: Random a => IO [a]
randomIOs = do
    len <- randomRIO (0, 20)
    sequence $ replicate len randomIO
