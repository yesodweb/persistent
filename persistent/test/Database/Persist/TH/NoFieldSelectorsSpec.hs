{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 902
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
#endif
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Persist.TH.NoFieldSelectorsSpec where

import TemplateTestImports

#if __GLASGOW_HASKELL__ >= 902

mkPersist sqlSettings {mpsFieldLabelModifier = const id} [persistLowerCase|
User
    ident Text
    name Text
    Primary ident
    team TeamId
    type Text

Team
    name Text
|]

spec :: Spec
spec = it "compiles" True

#else

spec :: Spec
spec = do
    it "only works with GHC 9.2 or greater" $ do
        pendingWith "only works with GHC 9.2 or greater"

#endif
