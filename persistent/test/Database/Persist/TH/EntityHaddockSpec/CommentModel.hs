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

{-# OPTIONS_GHC -haddock #-}

module Database.Persist.TH.EntityHaddockSpec.CommentModel
    ( CommentModel (..)
    ) where

import TemplateTestImports

mkPersist (sqlSettings {mpsEntityHaddocks = True}) [persistLowerCase|

-- | Doc comments work.
-- | Has multiple lines.
CommentModel
    -- | First line of comment on column.
    -- | Second line of comment on column.
    name String

    deriving Eq Show

|]
