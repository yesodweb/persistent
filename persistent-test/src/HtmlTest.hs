{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module HtmlTest (specs, specsWith) where

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

specsWith
    ::
    ( MonadFail m, MonadIO m
    , PersistEntityBackend entity ~ BaseBackend backend
    , PersistEntity entity
    , PersistStoreWrite backend
    )
    => RunDb backend m
    -> Maybe (ReaderT backend m a)
    -> (Html -> entity)
    -> (entity -> Html)
    -> Spec
specsWith runConn mmigrate htmlTable getHtmlFrom = describe "html" $ do
    it "works" $ asIO $ runConn $ do
        sequence_ mmigrate
        -- Ensure reading the data from the database works...
        sequence_ mmigrate

        sequence_ $ replicate 1000 $ do
            x <- liftIO randomValue
            key <- insert $ htmlTable x
            Just htmlTableY <- get key
            liftIO $ do
                renderHtml x @?= renderHtml (getHtmlFrom htmlTableY)

specs :: Spec
specs =
    specsWith runConn (Just (runMigrationSilent htmlMigrate)) HtmlTable htmlTableHtml

randomValue :: IO Html
randomValue =
                preEscapedToMarkup
              . T.pack
              . filter ((`notElem` forbidden) . generalCategory)
              . filter (<= '\xFFFF') -- only BMP
              . filter (/= '\0')     -- no nulls
         <$> randomIOs
    where forbidden = [NotAssigned, PrivateUse]

randomIOs :: Random a => IO [a]
randomIOs = do
    len <- randomRIO (0, 20)
    sequence $ replicate len randomIO
