{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Join
    ( selectOneMany
    ) where

import Database.Persist.Base
import Control.Monad (forM, liftM)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.List (foldl')

selectOneMany :: (PersistEntity one, PersistEntity many, PersistBackend m, Ord (Key one))
              => [Filter one]  -> [Order one]
              -> [Filter many] -> [Order many]
              -> ([Key one] -> Filter many)
              -> (many -> Key one) -- FIXME we can make this redundant by adding another function on PersistEntity: (b -> Filter a) -> a -> b... not sure if that will really work
              -> Bool -- ^ include ones without matching manys?
              -> m [((Key one, one), [(Key many, many)])]
selectOneMany oneF oneO manyF manyO eq getKey isOuter = do
    x <- selectList oneF oneO 0 0
    -- FIXME use select instead of selectList
    y <- selectList (eq (map fst x) : manyF) manyO 0 0
    let y' = foldl' go Map.empty y
    return $ mapMaybe (go' y') x
  where
    go m many@(_, many') =
        Map.insert (getKey many')
        (case Map.lookup (getKey many') m of
            Nothing -> (:) many
            Just v -> v . (:) many
            ) m
    go' y' one@(k, _) =
        case Map.lookup k y' of
            Nothing ->
                if isOuter
                    then Just (one, [])
                    else Nothing
            Just many -> Just (one, many [])
