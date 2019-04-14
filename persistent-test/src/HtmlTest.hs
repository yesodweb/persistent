{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module HtmlTest (specsWith, cleanDB, htmlMigrate) where

import Data.Char (generalCategory, GeneralCategory(..))
import qualified Data.Text as T
import System.Random (randomIO, randomRIO, Random)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text

import Init

-- Test lower case names
share [mkPersist persistSettings { mpsGeneric = True }, mkMigrate "htmlMigrate"] [persistLowerCase|
HtmlTable
    html Html
    deriving
|]

cleanDB :: Runner backend m => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter (HtmlTableGeneric backend)])

specsWith
    :: Runner backend m
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
