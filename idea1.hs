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
    data Field val
    data Filter val
    -- something about unique?

    insert :: val -> m (Key val)
    replace :: Key val -> val -> m ()

    -- FIXME update :: Key val -> [Field val] -> m ()

    get :: Key val -> m (Maybe (val))
    -- FIXME select :: [Filter val] -> m ([(Key val, val)])

    delete :: Key val -> m ()

data Person = Person String Int
    deriving Show

instance (Monad m, Functor m) =>
         HasTable Person (S.StateT (Map.Map Int Person) m)
         where
    data Key Person = PersonId { unPersonId :: !Int }
    data Field Person = PersonName String | PersonAge Int
    data Filter Person = PersonNameF String

    insert p = do
        m <- S.get
        let pid = 1 + Map.foldrWithKey (\k _ k' -> max k k') 0 m
        S.put $ Map.insert pid p m
        return $ PersonId pid
    replace (PersonId pid) = S.modify . Map.insert pid

    get pid = Map.lookup (unPersonId pid) <$> S.get

    delete = S.modify . Map.delete . unPersonId

deriving instance Show (Key Person)

main = flip S.evalStateT (Map.empty :: Map.Map Int Person) $ do
    pid1 <- insert $ Person "Michael" 25
    mp1 <- get pid1
    liftIO $ print (pid1, mp1)
    replace pid1 $ Person "Michael" 26
    mp2 <- get pid1
    liftIO $ print (pid1, mp2)
