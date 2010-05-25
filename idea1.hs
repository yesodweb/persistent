{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

import qualified Control.Monad.Trans.State as S
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad.IO.Class (liftIO)

class Monad m => HasTable key m where
    data Value key
    data Field key
    data Filter key
    -- something about unique?

    insert :: Value key -> m key
    replace :: key -> Value key -> m ()

    -- FIXME update :: key -> [Field key] -> m ()

    get :: key -> m (Maybe (Value key))
    -- FIXME select :: [Filter key] -> m ([(key, Value key)])

    delete :: key -> m ()

newtype PersonId = PersonId Int
    deriving (Eq, Ord, Show, Num)

instance (Monad m, Functor m) =>
         HasTable PersonId (S.StateT (Map.Map PersonId (String, Int)) m)
         where
    data Value PersonId = Person String Int
    data Field PersonId = PersonName String | PersonAge Int
    data Filter PersonId = PersonNameF String

    insert (Person name age) = do
        m <- S.get
        let pid = 1 + Map.foldrWithKey (\k _ k' -> max k k') 0 m
        S.put $ Map.insert pid (name, age) m
        return pid
    replace pid (Person name age) = S.modify $ Map.insert pid (name, age)

    get pid = fmap (uncurry Person) . Map.lookup pid <$> S.get

    delete = S.modify . Map.delete

deriving instance Show (Value PersonId)

main = flip S.evalStateT (Map.empty :: Map.Map PersonId (String, Int)) $ do
    pid1 <- insert $ Person "Michael" 25
    mp1 <- get pid1
    liftIO $ print (pid1, mp1)
    replace pid1 $ Person "Michael" 26
    mp2 <- get pid1
    liftIO $ print (pid1, mp2)
