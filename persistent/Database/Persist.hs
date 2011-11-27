{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
module Database.Persist
    ( PersistField (..)
    , PersistEntity (..)
    , PersistStore (..)
    , PersistQuery (..)
    , Key (..)
    , selectList
    , insertBy
    , getJust
    , belongsTo
    , belongsToJust
    , getByValue
    , checkUnique
    , Update (..)
    , SelectOpt (..)
    , Filter (..)
    , (=.), (+=.), (-=.), (*=.), (/=.)
    , (==.), (!=.), (<.), (>.), (<=.), (>=.)
    , (<-.), (/<-.)
    , (||.)
    ) where

-- Export public items from Database.Persist.Base
-- Also defines Filter creation and composition operators.
import Database.Persist.Base
import Database.Persist.Query

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
f ==. a = Filter f (Left a) Eq
f !=. a = Filter f (Left a) Ne
f <. a = Filter f (Left a) Lt
f <=. a = Filter f (Left a) Le
f >. a = Filter f (Left a) Gt
f >=. a = Filter f (Left a) Ge

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
