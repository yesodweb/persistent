{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Database.Persist.Query.Join
    ( -- * Typeclass
      RunJoin (..)
      -- * One-to-many relation
    , SelectOneMany (..)
    , selectOneMany
    ) where

import Database.Persist.Store
import Database.Persist.Query.Internal
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Data.List (foldl')

class PersistQuery backend m => RunJoin a backend m where
    type Result a
    runJoin :: a -> backend m (Result a)

data SelectOneMany backend one many = SelectOneMany
    { somFilterOne :: [Filter one]
    , somOrderOne :: [SelectOpt one]
    , somFilterMany :: [Filter many]
    , somOrderMany :: [SelectOpt many]
    , somFilterKeys :: [Key backend one] -> Filter many
    , somGetKey :: many -> Key backend one
    , somIncludeNoMatch :: Bool
    }

selectOneMany :: ([Key backend one] -> Filter many) -> (many -> Key backend one) -> SelectOneMany backend one many
selectOneMany filts get' = SelectOneMany [] [] [] [] filts get' False
instance (PersistEntity one, PersistEntity many, Ord (Key backend one), PersistQuery backend monad, backend ~ PersistEntityBackend one, backend ~ PersistEntityBackend many)
    => RunJoin (SelectOneMany backend one many) backend monad where
    type Result (SelectOneMany backend one many) =
        [((Entity one), [(Entity many)])]
    runJoin (SelectOneMany oneF oneO manyF manyO eq getKey isOuter) = do
        x <- selectList oneF oneO
        -- FIXME use select instead of selectList
        y <- selectList (eq (map entityKey x) : manyF) manyO
        let y' = foldl' go Map.empty y
        return $ mapMaybe (go' y') x
      where
        go m many@(Entity _ many') =
            Map.insert (getKey many')
            (case Map.lookup (getKey many') m of
                Nothing -> (:) many
                Just v -> v . (:) many
                ) m
        go' y' one@(Entity k _) =
            case Map.lookup k y' of
                Nothing ->
                    if isOuter
                        then Just (one, [])
                        else Nothing
                Just many -> Just (one, many [])
