{-# language UndecidableInstances #-}

-- | this just needs to compile
module PersistentTestModelsImports where

import Database.Persist.TH

share [mkPersist sqlSettings] [persistUpperCase|

User
    name    String
    age     Int
    deriving Eq Show

|]
