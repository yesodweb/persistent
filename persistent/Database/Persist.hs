{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist
    ( module Database.Persist.Class
    , module Database.Persist.Types

    -- * Reference Schema & Dataset
    -- |
    --
    -- All the combinators present here will be explained based on this schema:
    --
    -- > share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    -- > User
    -- >     name String
    -- >     age Int
    -- >     deriving Show
    -- > |]
    --
    -- and this dataset. The examples below will refer to this as dataset-1.
    --
    -- > +-----+-----+-----+
    -- > |id   |name |age  |
    -- > +-----+-----+-----+
    -- > |1    |SPJ  |40   |
    -- > +-----+-----+-----+
    -- > |2    |Simon|41   |
    -- > +-----+-----+-----+

    -- * Query update combinators
    , (=.), (+=.), (-=.), (*=.), (/=.)

      -- * Query filter combinators
    , (==.), (!=.), (<.), (>.), (<=.), (>=.), (<-.), (/<-.), (||.)

      -- * JSON Utilities
    , listToJSON
    , mapToJSON
    , toJsonText
    , getPersistMap

      -- * Other utilities
    , limitOffsetOrder
    ) where

import Database.Persist.Types
import Database.Persist.Class
import Database.Persist.Class.PersistField (getPersistMap)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson (toJSON, ToJSON)
#if MIN_VERSION_aeson(0, 7, 0)
import Data.Aeson.Encode (encodeToTextBuilder)
#else
import Data.Aeson.Encode (fromValue)
#endif

infixr 3 =., +=., -=., *=., /=.
(=.), (+=.), (-=.), (*=.), (/=.) ::
  forall v typ.  PersistField typ => EntityField v typ -> typ -> Update v

-- | Assign a field a value.
--
-- === __Example usage__
--
-- @
-- updateAge :: MonadIO m => ReaderT SqlBackend m ()
-- updateAge = updateWhere [UserName ==. \"SPJ\" ] [UserAge =. 45]
-- @
--
-- Similar to `updateWhere` which is shown in the above example you can use other functions present in the module "Database.Persist.Class". Note that the first parameter of `updateWhere` is [`Filter` val] and second parameter is [`Update` val]. By comparing this with the type of `==.` and `=.`, you can see that they match up in the above usage.
--
-- The above query when applied on "dataset-1", will produce this:
-- 
-- > +-----+-----+-----+
-- > |id   |name |age  |
-- > +-----+-----+-----+
-- > |1    |SPJ  |45   |
-- > +-----+-----+-----+
-- > |2    |Simon|41   |
-- > +-----+-----+-----+

f =. a = Update f a Assign

-- | Assign a field by addition (@+=@).
f +=. a = Update f a Add

-- | Assign a field by subtraction (@-=@).
f -=. a = Update f a Subtract

-- | Assign a field by multiplication (@*=@).
f *=. a = Update f a Multiply

-- | Assign a field by division (@/=@).
f /=. a = Update f a Divide

infix 4 ==., <., <=., >., >=., !=.
(==.), (!=.), (<.), (<=.), (>.), (>=.) ::
  forall v typ.  PersistField typ => EntityField v typ -> typ -> Filter v

-- | Check for equality.
f ==. a  = Filter f (Left a) Eq

-- | Non-equality check.
f !=. a = Filter f (Left a) Ne

-- | Less-than check.
f <. a  = Filter f (Left a) Lt

-- | Less-than or equal check.
f <=. a  = Filter f (Left a) Le

-- | Greater-than check.
f >. a  = Filter f (Left a) Gt

-- | Greater-than or equal check.
f >=. a  = Filter f (Left a) Ge

infix 4 <-., /<-.
(<-.), (/<-.) :: forall v typ.  PersistField typ => EntityField v typ -> [typ] -> Filter v

-- | Check if value is in given list.
f <-. a = Filter f (Right a) In

-- | Check if value is not in given list.
f /<-. a = Filter f (Right a) NotIn

infixl 3 ||.
(||.) :: forall v. [Filter v] -> [Filter v] -> [Filter v]

-- | The OR of two lists of filters. For example:
--
-- > selectList
-- >     ([ PersonAge >. 25
-- >      , PersonAge <. 30 ] ||.
-- >      [ PersonIncome >. 15000
-- >      , PersonIncome <. 25000 ])
-- >     []
--
-- will filter records where a person's age is between 25 and 30 /or/ a
-- person's income is between (15000 and 25000).
--
-- If you are looking for an @(&&.)@ operator to do @(A AND B AND (C OR D))@
-- you can use the @(++)@ operator instead as there is no @(&&.)@. For
-- example:
--
-- > selectList
-- >     ([ PersonAge >. 25
-- >      , PersonAge <. 30 ] ++
-- >     ([PersonCategory ==. 1] ||.
-- >      [PersonCategory ==. 5]))
-- >     []
--
-- will filter records where a person's age is between 25 and 30 /and/
-- (person's category is either 1 or 5).
a ||. b = [FilterOr  [FilterAnd a, FilterAnd b]]

-- | Convert list of 'PersistValue's into textual representation of JSON
-- object. This is a type-constrained synonym for 'toJsonText'.
listToJSON :: [PersistValue] -> T.Text
listToJSON = toJsonText

-- | Convert map (list of tuples) into textual representation of JSON
-- object. This is a type-constrained synonym for 'toJsonText'.
mapToJSON :: [(T.Text, PersistValue)] -> T.Text
mapToJSON = toJsonText

-- | A more general way to convert instances of `ToJSON` type class to
-- strict text 'T.Text'.
toJsonText :: ToJSON j => j -> T.Text
#if MIN_VERSION_aeson(0, 7, 0)
toJsonText = toStrict . toLazyText . encodeToTextBuilder . toJSON
#else
toJsonText = toStrict . toLazyText . fromValue . toJSON
#endif

-- | FIXME What's this exactly?
limitOffsetOrder :: PersistEntity val
  => [SelectOpt val]
  -> (Int, Int, [SelectOpt val])
limitOffsetOrder opts =
    foldr go (0, 0, []) opts
  where
    go (LimitTo l) (_, b, c) = (l, b ,c)
    go (OffsetBy o) (a, _, c) = (a, o, c)
    go x (a, b, c) = (a, b, x : c)
