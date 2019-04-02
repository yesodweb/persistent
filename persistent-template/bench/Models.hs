module Models where

import Data.Monoid
import Data.List
import Data.FileEmbed

-- | # of models, # of fields
mkModels :: Int -> Int -> String
mkModels = mkModelsWithFieldModifier id

mkNullableModels :: Int -> Int -> String
mkNullableModels = mkModelsWithFieldModifier maybeFields

mkModelsWithFieldModifier :: (String -> String) -> Int -> Int -> String
mkModelsWithFieldModifier k i f =
    unlines . fmap unlines . take i . map mkModel . zip [0..] . cycle $
        [ "Model"
        , "Foobar"
        , "User"
        , "King"
        , "Queen"
        , "Dog"
        , "Cat"
        ]
  where
    mkModel (i, m) =
        (m <> show i) : indent 4 (map k (mkFields f))

indent :: Int -> [String] -> [String]
indent i = map (replicate i ' ' ++)

mkFields :: Int -> [String]
mkFields i = take i $ map mkField $ zip [0..] $ cycle
    [ "Bool"
    , "Int"
    , "String"
    , "Char"
    , "Double"
    ]
  where
    mkField (i, typ) = "field" <> show i <> "\t\t" <> typ

maybeFields :: String -> String
maybeFields = (++ " Maybe")
