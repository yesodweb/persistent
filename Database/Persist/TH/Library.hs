{-# LANGUAGE CPP #-}
#include "cabal_macros.h"
module Database.Persist.TH.Library
    ( apE
    ) where

import Control.Applicative

#if !MIN_VERSION_base(4,3,0)
instance Applicative (Either x) where
    pure                      = Right
    l@(Left x) <*> _          = l
    _          <*> l@(Left _) = l
    Right f    <*> Right y    = Right (f y)
#endif // MIN_VERSION_base(4,3,0)

apE :: Either x (y -> z) -> Either x y -> Either x z
apE = (<*>)
