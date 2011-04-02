module Database.Persist.Join
    ( selectOneMany
    ) where

import Database.Persist.Base
import Control.Monad (forM, liftM)
import Data.Maybe (catMaybes)

selectOneMany :: (PersistEntity one, PersistEntity many, PersistBackend m)
              => [Filter one]  -> [Order one]
              -> [Filter many] -> [Order many]
              -> (Key one -> Filter many)
              -> m [((Key one, one), [(Key many, many)])]
selectOneMany oneF oneO manyF manyO eq = do
    x <- selectList oneF oneO 0 0
    catMaybes `liftM` forM x (\(k, v) -> do
        y <- selectList (eq k : manyF) manyO 0 0
        return $ if null y then Nothing else Just ((k, v), y))
