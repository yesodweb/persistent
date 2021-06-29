{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.CommentSpec where

import TemplateTestImports

import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

import Database.Persist.ImplicitIdDef
import Database.Persist.ImplicitIdDef.Internal (fieldTypeFromTypeable)
import Database.Persist.Types
import Database.Persist.Types
import Database.Persist.EntityDef
import Database.Persist.EntityDef.Internal (EntityDef(..))
import Database.Persist.FieldDef.Internal (FieldDef(..))

mkPersist sqlSettings [persistLowerCase|

-- | Doc comments work.
-- | Has multiple lines.
CommentModel
    -- | First line of comment on column
    -- | Second line of comment on column
    name String

    deriving Eq Show

|]

pass :: IO ()
pass = pure ()

asIO :: IO a -> IO a
asIO = id

spec :: Spec
spec = fdescribe "CommentSpec" $ do
    let
        ed =
            entityDef (Proxy @CommentModel)
    it "has entity comments" $ do
        entityComments ed
            `shouldBe` do
                Just $ mconcat
                    [ "Doc comments work.\n"
                    , "Has multiple lines.\n"
                    ]

    describe "fieldComments" $ do
        let
            [nameComments] =
                map fieldComments $ entityFields ed
        it "has the right name comments" $ do
            nameComments
                `shouldBe` do
                    Just $ mconcat
                        [ "First line of comment on column.\n"
                        , "Second line of comment on column.\n"
                        ]
