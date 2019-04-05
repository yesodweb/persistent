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
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings,  mkMigrate "customFieldMigrate"] [persistLowerCase|
#endif
  BlogPost
    article Markdown
    deriving Show Eq
|]

specs :: Spec
specs = specsWith db BlogPost

specsWith
    ::
    ( PersistEntityBackend entity ~ BaseBackend backend
    , Show entity, Eq entity
    , PersistStoreRead backend
    , PersistStoreWrite backend
    , MonadIO m
    , MonadFail m
    , PersistEntity entity
    )
    => (ReaderT backend m () -> IO ())
    -> (Markdown -> entity)
    -> Spec
specsWith runDB blogPost = describe "Custom persist field" $ do
  it "should read what it wrote" $ runDB $ do
    let originalBlogPost = blogPost "article"
    blogPostId <- insert originalBlogPost
    Just newBlogPost <- get blogPostId
    liftIO $ originalBlogPost @?= newBlogPost
