module Main (main) where

import Criterion.Main
import qualified Data.Text as Text
import Language.Haskell.TH

import Database.Persist.Quasi
import Database.Persist.TH
import Models

parseReferences' :: String -> IO Exp
parseReferences' = runQ . parseReferences upperCaseSettings . Text.pack

main :: IO ()
main = defaultMain
    [ bgroup "Parsing"
        [ bgroup "Non-Null Fields"
            [ bgroup "Increasing model count"
                [ bench "1x10" $ whnfIO $ parseReferences' (mkModels 10 10)
                , bench "10x10" $ whnfIO $ parseReferences' (mkModels 10 10)
                , bench "100x10" $ whnfIO $ parseReferences' (mkModels 100 10)
                , bench "1000x10" $ whnfIO $ parseReferences' (mkModels 1000 10)
                ]
            , bgroup "Increasing field count"
                [ bench "100x1" $ whnfIO $ parseReferences' (mkModels 100 1)
                , bench "100x10" $ whnfIO $ parseReferences' (mkModels 100 10)
                , bench "100x100" $ whnfIO $ parseReferences' (mkModels 100 100)
                , bench "100x1000" $ whnfIO $ parseReferences' (mkModels 100 1000)
                ]
            ]
        , bgroup "Nullable"
            [ bgroup "Increasing model count"
                [ bench "1x10" $ whnfIO $ parseReferences' (mkNullableModels 10 10)
                , bench "10x10" $ whnfIO $ parseReferences' (mkNullableModels 10 10)
                , bench "100x10" $ whnfIO $ parseReferences' (mkNullableModels 100 10)
                , bench "1000x10" $ whnfIO $ parseReferences' (mkNullableModels 1000 10)
                ]
            , bgroup "Increasing field count"
                [ bench "100x1" $ whnfIO $ parseReferences' (mkNullableModels 100 1)
                , bench "100x10" $ whnfIO $ parseReferences' (mkNullableModels 100 10)
                , bench "100x100" $ whnfIO $ parseReferences' (mkNullableModels 100 100)
                , bench "100x1000" $ whnfIO $ parseReferences' (mkNullableModels 100 1000)
                ]
            ]
        ]
    ]
