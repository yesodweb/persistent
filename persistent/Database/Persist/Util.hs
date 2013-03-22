{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Util
    ( nullable
    , IsNullable(..)
    , WhyNullable(..)
    , deprecate
    ) where

import System.IO.Unsafe (unsafePerformIO)
import Data.Text
import Database.Persist.Types

nullable :: [Text] -> IsNullable
nullable s
    | "Maybe"    `elem` s = Nullable ByMaybeAttr
    | "nullable" `elem` s = Nullable ByNullableAttr
    | "null"     `elem` s = deprecate "Please replace null with Maybe" (Nullable ByMaybeAttr)
    | otherwise = NotNullable

deprecate :: String -> a -> a
deprecate s x = unsafePerformIO $ do
    putStrLn $ "DEPRECATED: " ++ s
    return x
