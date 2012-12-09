{-# LANGUAGE CPP #-}
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
apE (Left x)   _         = Left x
apE _          (Left x)  = Left x
apE (Right f)  (Right y) = Right (f y)
#endif
