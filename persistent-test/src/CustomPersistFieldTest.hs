{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# language RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module CustomPersistFieldTest (specsWith, customFieldMigrate) where

import Init
import CustomPersistField
import Control.Monad.Fail

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
