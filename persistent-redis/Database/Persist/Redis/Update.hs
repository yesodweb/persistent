module Database.Persist.Redis.Update
    ( cmdUpdate
    ) where

import Control.Exception (throw)
import Data.Either()
import Data.Functor.Identity
import Data.Functor.Constant

import Database.Persist
import Database.Persist.Redis.Exception

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity . l (\_ -> Identity b)

type Getting r s t a b = (a -> Constant r b) -> s -> Constant r t

view :: s -> Getting a s t a b -> a
view s l = getConstant (l Constant s)

cmdUpdate :: PersistEntity val => Entity val -> [Update val] -> Entity val
cmdUpdate = foldr updateOneField

updateOneField :: PersistEntity val => Update val -> Entity val -> Entity val
updateOneField (BackendUpdate _) _ =  throw $ NotSupportedOperation "Backend specific update"
updateOneField (Update field v Assign) oldValue  = set (fieldLens field) v oldValue
updateOneField (Update _ _ (BackendSpecificUpdate _)) _ =
    throw $ NotSupportedOperation "Backend specific update withing update operation"

updateOneField (Update field v up) oldValue  = set (fieldLens field) newValue oldValue
    where
        lens = fieldLens field
        pv    = toPersistValue v
        oldV = toPersistValue $ view oldValue lens
        eitherNewValue = fromPersistValue $ apply up oldV pv
        newValue = either (\_ -> throw IncorrectBehavior) id eitherNewValue


apply :: PersistUpdate -> PersistValue -> PersistValue -> PersistValue

apply Assign _ _ = throw IncorrectBehavior

apply Add (PersistInt64 x) (PersistInt64 y) = PersistInt64 (x + y)
apply Add (PersistDouble x) (PersistDouble y) = PersistDouble (x + y)
apply Add (PersistRational x) (PersistRational y) = PersistRational (x + y)
apply Add _ _ = throw $ IncorrectUpdate "Unable to apply addition to this field"

apply Subtract (PersistInt64 x) (PersistInt64 y) = PersistInt64 (x - y)
apply Subtract (PersistDouble x) (PersistDouble y) = PersistDouble (x - y)
apply Subtract (PersistRational x) (PersistRational y) = PersistRational (x - y)
apply Subtract _ _ = throw $ IncorrectUpdate "Unable to apply subtraction to this field"

apply Multiply (PersistInt64 x) (PersistInt64 y) = PersistInt64 (x * y)
apply Multiply (PersistDouble x) (PersistDouble y) = PersistDouble (x * y)
apply Multiply (PersistRational x) (PersistRational y) = PersistRational (x * y)
apply Multiply _ _ = throw $ IncorrectUpdate "Unable to apply subtraction to this field"

apply Divide (PersistInt64 x) (PersistInt64 y) = PersistInt64 (div x y)
apply Divide (PersistDouble x) (PersistDouble y) = PersistDouble (x / y)
apply Divide (PersistRational x) (PersistRational y) = PersistRational (x / y)
apply Divide _ _ = throw $ IncorrectUpdate "Unable to apply subtraction to this field"

apply (BackendSpecificUpdate _) _ _ = throw IncorrectBehavior