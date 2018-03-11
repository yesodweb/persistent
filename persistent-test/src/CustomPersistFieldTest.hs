{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE CPP #-}
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

module CustomPersistFieldTest (
  specs
#ifndef WITH_NOSQL
  , customFieldMigrate
#endif
) where

import Init
import CustomPersistField

#ifdef WITH_NOSQL
db :: Action IO () -> Assertion
db = db' (return ())
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings,  mkMigrate "customFieldMigrate"] [persistLowerCase|
#endif
  BlogPost
    article Markdown
    deriving Show Eq
|]

specs :: Spec
specs = describe "Custom persist field" $ do
  it "should read what it wrote" $ db $ do
    let originalBlogPost = BlogPost "article"
    blogPostId <- insert originalBlogPost
    Just newBlogPost <- get blogPostId
    liftIO $ originalBlogPost @?= newBlogPost

