{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist
    ( module Database.Persist.Class
    , module Database.Persist.Types

      -- * Store functions
    , insertBy
    , getJust
    , belongsTo
    , belongsToJust
    , getByValue

      -- * Query functions
    , selectList
    , selectKeysList
    , deleteCascadeWhere

      -- * query combinators
    , (=.), (+=.), (-=.), (*=.), (/=.)
    , (==.), (!=.), (<.), (>.), (<=.), (>=.)
    , (<-.), (/<-.)
    , (||.)

      -- * JSON Utilities
    , listToJSON
    , mapToJSON
    , getPersistMap

      -- * Other utililities
    , limitOffsetOrder
    ) where

import Database.Persist.Types
import Database.Persist.Class
import Database.Persist.Class.PersistField (getPersistMap)
import qualified Data.Text as T
import qualified Control.Exception as E
import Control.Monad (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson (toJSON)
import Data.Aeson.Encode (fromValue)

-- | Insert a value, checking for conflicts with any unique constraints.  If a
-- duplicate exists in the database, it is returned as 'Left'. Otherwise, the
-- new 'Key is returned as 'Right'.
insertBy :: (PersistEntity v, PersistStore m, PersistUnique m, PersistMonadBackend m ~ PersistEntityBackend v)
          => v -> m (Either (Entity v) (Key v))
insertBy val =
    go $ persistUniqueKeys val
  where
    go [] = Right `liftM` insert val
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just z -> return $ Left z

-- | A modification of 'getBy', which takes the 'PersistEntity' itself instead
-- of a 'Unique' value. Returns a value matching /one/ of the unique keys. This
-- function makes the most sense on entities with a single 'Unique'
-- constructor.
getByValue :: (PersistEntity v, PersistUnique m, PersistEntityBackend v ~ PersistMonadBackend m)
           => v -> m (Maybe (Entity v))
getByValue val =
    go $ persistUniqueKeys val
  where
    go [] = return Nothing
    go (x:xs) = do
        y <- getBy x
        case y of
            Nothing -> go xs
            Just z -> return $ Just z

-- | curry this to make a convenience function that loads an associated model
--   > foreign = belongsTo foeignId
belongsTo ::
  (PersistStore m
  , PersistEntity ent1
  , PersistEntity ent2
  , PersistMonadBackend m ~ PersistEntityBackend ent2
  ) => (ent1 -> Maybe (Key ent2)) -> ent1 -> m (Maybe ent2)
belongsTo foreignKeyField model = case foreignKeyField model of
    Nothing -> return Nothing
    Just f -> get f

-- | same as belongsTo, but uses @getJust@ and therefore is similarly unsafe
belongsToJust ::
  (PersistStore m
  , PersistEntity ent1
  , PersistEntity ent2
  , PersistMonadBackend m ~ PersistEntityBackend ent2)
  => (ent1 -> Key ent2) -> ent1 -> m ent2
belongsToJust getForeignKey model = getJust $ getForeignKey model

-- | Same as get, but for a non-null (not Maybe) foreign key
--   Unsafe unless your database is enforcing that the foreign key is valid
getJust :: (PersistStore m, PersistEntity val, Show (Key val), PersistMonadBackend m ~ PersistEntityBackend val) => Key val -> m val
getJust key = get key >>= maybe
  (liftIO $ E.throwIO $ PersistForeignConstraintUnmet $ T.pack $ show key)
  return

infixr 3 =., +=., -=., *=., /=.
(=.), (+=.), (-=.), (*=.), (/=.) :: forall v typ.  PersistField typ => EntityField v typ -> typ -> Update v
-- | assign a field a value
f =. a = Update f a Assign
-- | assign a field by addition (+=)
f +=. a = Update f a Add
-- | assign a field by subtraction (-=)
f -=. a = Update f a Subtract
-- | assign a field by multiplication (*=)
f *=. a = Update f a Multiply
-- | assign a field by division (/=)
f /=. a = Update f a Divide

infix 4 ==., <., <=., >., >=., !=.
(==.), (!=.), (<.), (<=.), (>.), (>=.) ::
  forall v typ.  PersistField typ => EntityField v typ -> typ -> Filter v
f ==. a  = Filter f (Left a) Eq
f !=. a = Filter f (Left a) Ne
f <. a  = Filter f (Left a) Lt
f <=. a  = Filter f (Left a) Le
f >. a  = Filter f (Left a) Gt
f >=. a  = Filter f (Left a) Ge

infix 4 <-., /<-.
(<-.), (/<-.) :: forall v typ.  PersistField typ => EntityField v typ -> [typ] -> Filter v
-- | In
f <-. a = Filter f (Right a) In
-- | NotIn
f /<-. a = Filter f (Right a) NotIn

infixl 3 ||.
(||.) :: forall v. [Filter v] -> [Filter v] -> [Filter v]
-- | the OR of two lists of filters
a ||. b = [FilterOr  [FilterAnd a, FilterAnd b]]

-- | Call 'selectSource' but return the result as a list.
selectList :: (PersistEntity val, PersistQuery m, PersistEntityBackend val ~ PersistMonadBackend m)
           => [Filter val]
           -> [SelectOpt val]
           -> m [Entity val]
selectList a b = selectSource a b C.$$ CL.consume

-- | Call 'selectKeys' but return the result as a list.
selectKeysList :: (PersistEntity val, PersistQuery m, PersistEntityBackend val ~ PersistMonadBackend m)
               => [Filter val]
               -> [SelectOpt val]
               -> m [Key val]
selectKeysList a b = selectKeys a b C.$$ CL.consume

deleteCascadeWhere :: (DeleteCascade a m, PersistQuery m)
                   => [Filter a] -> m ()
deleteCascadeWhere filts = selectKeys filts [] C.$$ CL.mapM_ deleteCascade

listToJSON :: [PersistValue] -> T.Text
listToJSON = toStrict . toLazyText . fromValue . toJSON

mapToJSON :: [(T.Text, PersistValue)] -> T.Text
mapToJSON = toStrict . toLazyText . fromValue . toJSON

limitOffsetOrder :: PersistEntity val => [SelectOpt val] -> (Int, Int, [SelectOpt val])
limitOffsetOrder opts =
    foldr go (0, 0, []) opts
  where
    go (LimitTo l) (_, b, c) = (l, b ,c)
    go (OffsetBy o) (a, _, c) = (a, o, c)
    go x (a, b, c) = (a, b, x : c)
