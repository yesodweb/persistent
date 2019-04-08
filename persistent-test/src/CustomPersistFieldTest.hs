{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# language RankNTypes #-}
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
  , specsWith
#ifndef WITH_NOSQL
  , customFieldMigrate
#endif
) where

import Init
import CustomPersistField
import Control.Monad.Fail

#ifdef WITH_NOSQL
db :: Action IO () -> Assertion
db = db' (return ())
mkPersist persistSettings { mpsGeneric = True }[persistUpperCase|
#else
share [mkPersist sqlSettings { mpsGeneric = True },  mkMigrate "customFieldMigrate"] [persistLowerCase|
#endif
  BlogPost
    article Markdown
    deriving Show Eq
|]

specs :: Spec
specs = specsWith db

specsWith
    ::
    ( PersistStoreRead backend
    , PersistStoreWrite backend
    , PersistStoreWrite (BaseBackend backend)
    , MonadIO m
    , MonadFail m
    )
    => RunDb backend m
    -> Spec
specsWith runDB = describe "Custom persist field" $ do
  it "should read what it wrote" $ runDB $ do
    let originalBlogPost = BlogPost "article"
    blogPostId <- insert originalBlogPost
    Just newBlogPost <- get blogPostId
    liftIO $ originalBlogPost @?= newBlogPost
