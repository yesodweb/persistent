{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Contrib.Orphans where
import Data.Monoid
import Text.Hamlet.Monad

instance Monad m => Monoid (Hamlet url m ()) where
    mempty = return ()
    mappend = (>>)
