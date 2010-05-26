{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

import qualified Control.Monad.Trans.State as S
import qualified Data.Map as Map
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Control.Arrow (first)

class Monad m => HasTable val m where
    data Key val
    -- something about unique?

    insert :: val -> m (Key val)
    replace :: Key val -> val -> m ()

    get :: Key val -> m (Maybe (val))

    delete :: Key val -> m ()

class HasTable val m => HasUpdateTable val m where
    update :: Key val -> [Field val] -> m ()

class HasTable val m => HasSelectTable val m where
    select :: [Filter val] -> m ([(Key val, val)])

data Person = Person String Int
    deriving Show

class HasField a where
    data Field a
    updateField :: Field a -> a -> a

instance HasField Person where
    data Field Person = PersonName String | PersonAge Int
    updateField (PersonName name) (Person _ age) = Person name age
    updateField (PersonAge age) (Person name _) = Person name age

class HasFilter a where
    data Filter a
    applyFilter :: Filter a -> a -> Bool

instance HasFilter Person where
    data Filter Person = PersonNameEq String | PersonAgeLt Int
    applyFilter (PersonNameEq x) (Person y _) = x == y
    applyFilter (PersonAgeLt x) (Person _ y) = y < x

instance (Monad m, Functor m) =>
         HasTable v (S.StateT (Map.Map Int v) m)
         where
    data Key v = MapKey { unMapKey :: !Int }
        deriving Show

    insert p = do
        m <- S.get
        let pid = 1 + Map.foldrWithKey (\k _ k' -> max k k') 0 m
        S.put $ Map.insert pid p m
        return $ MapKey pid
    replace (MapKey pid) = S.modify . Map.insert pid

    get pid = Map.lookup (unMapKey pid) <$> S.get

    delete = S.modify . Map.delete . unMapKey

instance (Functor m, Monad m, HasField v) => HasUpdateTable v (S.StateT (Map.Map Int v) m) where
    update (MapKey k) us = S.modify $ \m ->
        let moldVal = Map.lookup k m
         in case moldVal of
                Nothing -> m
                Just oldVal ->
                    let newVal = foldr updateField oldVal us
                     in Map.insert k newVal m

instance (Functor m, Monad m, HasFilter v) => HasSelectTable v (S.StateT (Map.Map Int v) m) where
    select fs = map (first MapKey) . filter go . Map.toList <$> S.get
      where
        go (_, val) = all (flip applyFilter val) fs

main = flip S.evalStateT (Map.empty :: Map.Map Int Person) $ do
    pid1 <- insert $ Person "Michael" 25
    mp1 <- get pid1
    liftIO $ print mp1
    replace pid1 $ Person "Michael" 26
    mp2 <- get pid1
    liftIO $ print mp2
    update pid1 [PersonAge 27]
    mp3 <- get pid1
    liftIO $ print mp3
    replace pid1 $ Person "Michael" 28
    mp4 <- get pid1
    liftIO $ print mp4
    p5s <- select [PersonNameEq "Michael"]
    liftIO $ print p5s
    p6s <- select [PersonAgeLt 27]
    liftIO $ print p6s
    p7s <- select [PersonAgeLt 29]
    liftIO $ print p7s
    p8s <- select [PersonNameEq "Michael", PersonAgeLt 29]
    liftIO $ print p8s
    p9s <- select [PersonNameEq "Michael", PersonAgeLt 27]
    liftIO $ print p9s
    delete pid1
    mplast <- get pid1
    liftIO $ print mplast
