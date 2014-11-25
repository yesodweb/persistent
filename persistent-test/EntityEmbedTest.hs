{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, CPP, GADTs, TypeFamilies, OverloadedStrings, FlexibleContexts, EmptyDataDecls, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
module EntityEmbedTest where

-- because we are using a type alias we need to declare in a separate module
-- this is used in EmbedTest
#if WITH_NOSQL
import Init

mkPersist persistSettings [persistUpperCase|
  ARecord
    name Text
    deriving Show Eq Read Ord
|]

type AnEntity = Entity ARecord
#endif
