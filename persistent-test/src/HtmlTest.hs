{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module HtmlTest (specs, specsWith, cleanDB, htmlMigrate) where

import Database.Persist.TH
import Data.Char (generalCategory, GeneralCategory(..))
import qualified Data.Text as T
import System.Random (randomIO, randomRIO, Random)
import Control.Monad.Trans.Resource (runResourceT)

import Init
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

-- Test lower case names
share [mkPersist persistSettings { mpsGeneric = True }, mkMigrate "htmlMigrate"] [persistLowerCase|
HtmlTable
    html Html
    deriving
|]

cleanDB :: (MonadIO m, PersistQueryWrite backend, PersistStoreWrite (BaseBackend backend)) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter (HtmlTableGeneric backend)])

specsWith
    ::
    ( MonadFail m, MonadIO m
    , PersistStoreWrite backend
    , BaseBackend backend ~ backend
    )
    => RunDb backend m
    -> Maybe (ReaderT backend m a)
    -> Spec
specsWith runConn mmigrate = describe "html" $ do
    it "works" $ asIO $ runConn $ do
        sequence_ mmigrate
        -- Ensure reading the data from the database works...
        sequence_ mmigrate

        sequence_ $ replicate 1000 $ do
            x <- liftIO randomValue
            key <- insert $ HtmlTable x
            Just htmlTableY <- get key
            liftIO $ do
                renderHtml x @?= renderHtml (htmlTableHtml htmlTableY)

specs :: Spec
specs =
    specsWith runConn (Just (runMigrationSilent htmlMigrate))

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
