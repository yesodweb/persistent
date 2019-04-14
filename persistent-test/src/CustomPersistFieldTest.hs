{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module CustomPersistFieldTest (specsWith, customFieldMigrate) where

import CustomPersistField
import Init

share [mkPersist sqlSettings { mpsGeneric = True },  mkMigrate "customFieldMigrate"] [persistLowerCase|
  BlogPost
    article Markdown
    deriving Show Eq
|]

specsWith :: Runner backend m => RunDb backend m -> Spec
specsWith runDB = describe "Custom persist field" $ do
  it "should read what it wrote" $ runDB $ do
    let originalBlogPost = BlogPost "article"
    blogPostId <- insert originalBlogPost
    Just newBlogPost <- get blogPostId
    liftIO $ originalBlogPost @?= newBlogPost
