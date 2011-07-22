module Database.Persist
    ( PersistField (..)
    , PersistEntity (..)
    , PersistBackend (..)
    , Key (..)
    , selectList
    , selectFirst
    , insertBy
    , getByValue
    , checkUnique
    , Update (..)
    , SelectOpt (..)
    , Filter (..)
    , (=.), (+.)
    , (==.), (/=.), (<.), (>.), (<=.), (>=.)
    , (<-.), (/<-.)
    , (&&.), (||.)
    ) where

import Database.Persist.Base

infixr 3 =., +.
f =. a = Update f a Assign
f +. a = Update f a Add

infix 4 ==., <., <=., >., >=., /=.
f ==. a = Filter f (Left a) Eq
f /=. a = Filter f (Left a) Ne
f <. a = Filter f (Left a) Lt
f <=. a = Filter f (Left a) Le
f >. a = Filter f (Left a) Gt
f >=. a = Filter f (Left a) Ge

infix 4 <-., /<-.
f <-. a = Filter f (Right a) In
f /<-. a = Filter f (Right a) NotIn

infixl 3 &&., ||.
a &&. b = [FilterAnd [FilterAnd a, FilterAnd b]]
a ||. b = [FilterOr  [FilterAnd a, FilterAnd b]]
