{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -haddock #-}

module Database.Persist.TH.EntityHaddockSpec (spec) where

import TemplateTestImports
import Database.Persist.TH.EntityHaddockSpec.CommentModel

#if MIN_VERSION_template_haskell(2,18,0)
import Language.Haskell.TH (DocLoc (DeclDoc), getDoc)
import Language.Haskell.TH.Syntax (lift)

[d|
    commentModelDoc :: Maybe String
    commentModelDoc = $(lift =<< getDoc (DeclDoc ''CommentModel))

    commentFieldDoc :: Maybe String
    commentFieldDoc = $(lift =<< getDoc (DeclDoc 'commentModelName))
    |]

spec :: Spec
spec = describe "EntityHaddockSpec" $ do
    it "generates entity Haddock" $ do
        let expected = unlines [ "Doc comments work."
                               , "Has multiple lines."
                               ]
        commentModelDoc `shouldBe` Just expected
    it "generates field Haddock" $ do
        let expected = unlines [ "First line of comment on column."
                               , "Second line of comment on column."
                               ]
        commentFieldDoc `shouldBe` Just expected
#else
spec :: Spec
spec = pure ()
#endif
