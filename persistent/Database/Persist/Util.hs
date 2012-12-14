{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Util
    ( nullable
    , IsNullable(..)
    , WhyNullable(..)
    , deprecate
    ) where

import System.IO.Unsafe (unsafePerformIO)
import Data.Text

nullable :: [Text] -> IsNullable
nullable s
    | "Maybe"    `elem` s = Nullable ByMaybeAttr
    | "nullable" `elem` s = Nullable ByNullableAttr
    | "null"     `elem` s = deprecate "Please replace null with Maybe" (Nullable ByMaybeAttr)
    | otherwise = NotNullable


data IsNullable = Nullable !WhyNullable
                | NotNullable
                  deriving (Eq, Show)

-- | The reason why a field is 'nullable' is very important.  A
-- field that is nullable because of a @Maybe@ tag will have its
-- type changed from @A@ to @Maybe A@.  OTOH, a field that is
-- nullable because of a @nullable@ tag will remain with the same
-- type.
data WhyNullable = ByMaybeAttr
                 | ByNullableAttr
                  deriving (Eq, Show)


deprecate :: String -> a -> a
deprecate s x = unsafePerformIO $ do
    putStrLn $ "DEPRECATED: " ++ s
    return x
