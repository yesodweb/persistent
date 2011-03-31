module Database.Persist.Util
    ( nullable
    , deprecate
    ) where

import System.IO.Unsafe (unsafePerformIO)

nullable :: [String] -> Bool
nullable s
    | "Maybe" `elem` s = True
    | "null" `elem` s = deprecate "Please replace null with Maybe" True
    | otherwise = False

deprecate :: String -> a -> a
deprecate s x = unsafePerformIO $ do
    putStrLn $ "DEPRECATED: " ++ s
    return x
