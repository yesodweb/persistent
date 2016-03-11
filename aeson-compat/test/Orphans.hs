{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif

import Data.Vector as V
import Data.Version (Version(..))
import Test.Tasty.QuickCheck

#if !(MIN_VERSION_QuickCheck(2,8,0) && MIN_VERSION_base(4,8,0))
import Numeric.Natural (Natural)
#endif

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink    =  fmap V.fromList . shrink . V.toList

#if !(MIN_VERSION_QuickCheck(2,8,0) && MIN_VERSION_base(4,8,0))
instance Arbitrary Natural where
  arbitrary = fmap (fromInteger . abs) arbitrary
#endif

instance Arbitrary Version where
  arbitrary = do
    x <- fmap abs arbitrary
    xs <- (fmap . fmap) abs arbitrary
    return $ Version (x : xs) []
