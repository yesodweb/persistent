{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module HtmlTestMongo (specs) where

import Database.Persist.TH
import Data.Char (generalCategory, GeneralCategory(..))
import qualified Data.Text as T
import System.Random (randomIO, randomRIO, Random)
import Control.Monad.Trans.Resource (runResourceT)

import Init
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

-- Test lower case names
share [mkPersist persistSettings, mkMigrate "htmlMigrate"] [persistLowerCase|
HtmlTable
    html Html
    deriving
|]

cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend HtmlTable ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter HtmlTable])

specs :: Spec
specs = describe "html" $ do
    it "works" $ asIO $ runResourceT $ runConn $ do

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
