{-# LANGUAGE CPP #-}
#include "cabal_macros.h"
module Database.Persist.TH.Library
    ( apE
    ) where

#if MIN_VERSION_base(4,3,0)
import Control.Applicative
#endif

apE :: Either x (y -> z) -> Either x y -> Either x z

#if MIN_VERSION_base(4,3,0)
apE = (<*>)
#else
apE l@(Left x) _          = l
apE _          l@(Left _) = l
apE Right f    Right y    = Right (f y)
#endif
