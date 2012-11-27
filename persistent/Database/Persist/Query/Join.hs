{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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

class PersistQuery m => RunJoin a m where
    type Result a
    runJoin :: a -> m (Result a)

data SelectOneMany one many = SelectOneMany
    { somFilterOne :: [Filter one]
    , somOrderOne :: [SelectOpt one]
    , somFilterMany :: [Filter many]
    , somOrderMany :: [SelectOpt many]
    , somFilterKeys :: [Key one] -> Filter many
    , somGetKey :: many -> Key one
    , somIncludeNoMatch :: Bool
    }

selectOneMany :: ([Key one] -> Filter many) -> (many -> Key one) -> SelectOneMany one many
selectOneMany filts get' = SelectOneMany [] [] [] [] filts get' False

instance ( PersistEntity one
         , PersistEntity many
         , Ord (Key one)
         , PersistQuery monad
         , PersistMonadBackend monad ~ PersistEntityBackend one
         , PersistEntityBackend one ~ PersistEntityBackend many
         )
    => RunJoin (SelectOneMany one many) monad where
    type Result (SelectOneMany one many) =
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
