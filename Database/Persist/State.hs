{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Persist.State
    ( PersistState
    , runPersistState
    ) where

import Database.Persist
import Control.Monad.Trans.State hiding (get)
import qualified Control.Monad.Trans.State as S
import qualified Data.Map as Map

newtype PersistState val m a = PersistState
    { unPersistState :: StateT (Map.Map Int val) m a
    }

get' :: PersistState val m (Map.Map Int val)
get' = undefined

put' :: Map.Map Int val -> PersistState val m ()
put' = undefined

runPersistState :: PersistState val m a -> Map.Map Int val
                -> m (a, Map.Map Int val)
runPersistState = runStateT . unPersistState

instance PersistFamily val (PersistState val m)
      => Persist val (PersistState val m) where
    data Key val (PersistState val m) = StateKey { unStateKey :: Int }

    insert val = do
        m <- get'
        let k :: Int
            k = 1 + Map.foldrWithKey (\k _ k' -> max k k') 0 m
        put' $ Map.insert k val m
        return $ StateKey k
    replace (StateKey k) val = do
        m <- get'
        put' $ Map.insert k val m
    replaceBy   :: Unique val m   -> val            -> m (Key val m)
    {-
    update      :: Key val m      -> [Update val m] -> m ()
    delete      :: Key val m                        -> m ()
    deleteWhere :: [Filter val m]                   -> m ()
    -}
