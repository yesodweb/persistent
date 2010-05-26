{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

import qualified Control.Monad.Trans.State as S
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad.IO.Class (liftIO)

class Monad m => HasTable val m where
    data Key val
    -- something about unique?

    insert :: val -> m (Key val)
    replace :: Key val -> val -> m ()

    update :: Key val -> [Field val] -> m ()

    get :: Key val -> m (Maybe (val))
    -- FIXME select :: [Filter val] -> m ([(Key val, val)])

    delete :: Key val -> m ()

data family Filter val

data Person = Person String Int
    deriving Show
data instance Filter Person = PersonNameF String

class HasField a where
    data Field a
    updateField :: Field a -> a -> a

instance HasField Person where
    data Field Person = PersonName String | PersonAge Int
    updateField (PersonName name) (Person _ age) = Person name age
    updateField (PersonAge age) (Person name _) = Person name age

instance (Monad m, Functor m, HasField v) =>
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

    update (MapKey k) us = S.modify $ \m ->
        let moldVal = Map.lookup k m
         in case moldVal of
                Nothing -> m
                Just oldVal ->
                    let newVal = foldr updateField oldVal us
                     in Map.insert k newVal m

main = flip S.evalStateT (Map.empty :: Map.Map Int Person) $ do
    pid1 <- insert $ Person "Michael" 25
    mp1 <- get pid1
    liftIO $ print (pid1, mp1)
    replace pid1 $ Person "Michael" 26
    mp2 <- get pid1
    liftIO $ print (pid1, mp2)
    update pid1 [PersonAge 27]
    mp3 <- get pid1
    liftIO $ print (pid1, mp3)
