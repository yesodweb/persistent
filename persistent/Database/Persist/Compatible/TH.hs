{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Database.Persist.Compatible.TH
    ( makeCompatibleInstances
    , makeCompatibleKeyInstances
    ) where

import Data.Aeson
import Database.Persist.Class
import Database.Persist.Sql.Class
import Language.Haskell.TH

import Database.Persist.Compatible.Types

-- | Gives a bunch of useful instance declarations for a backend based on its
-- compatibility with another backend, using 'Compatible'.
--
-- The argument should be a type of the form @ forall v1 ... vn. Compatible b s @
-- (Quantification is optional, but supported because TH won't let you have
-- unbound type variables in a type splice). The instance is produced for @s@
-- based on the instance defined for @b@, which is constrained in the instance
-- head to exist.
--
-- @v1 ... vn@ are implicitly quantified in the instance, which is derived via
-- @'Compatible' b s@.
--
-- @since 2.12
makeCompatibleInstances :: Q Type -> Q [Dec]
makeCompatibleInstances compatibleType = do
        (b, s) <- compatibleType >>= \case
            ForallT _ _ (AppT (AppT (ConT conTName) b) s) ->
                if conTName == ''Compatible
                    then pure (b, s)
                    else fail $
                                "Cannot make `deriving via` instances if the argument is " <>
                                "not of the form `forall v1 ... vn. Compatible sub sup`"
            AppT (AppT (ConT conTName) b) s ->
                if conTName == ''Compatible
                    then pure (b, s)
                    else fail $
                                "Cannot make `deriving via` instances if the argument is " <>
                                "not of the form `Compatible sub sup`"
            _ -> fail $
                        "Cannot make `deriving via` instances if the argument is " <>
                        "not of the form `Compatible sub sup`"

        [d|
                deriving via (Compatible $(return b) $(return s)) instance (HasPersistBackend $(return b)) => HasPersistBackend $(return s)
                deriving via (Compatible $(return b) $(return s)) instance (HasPersistBackend $(return b), PersistStoreRead $(return b)) => PersistStoreRead $(return s)
                deriving via (Compatible $(return b) $(return s)) instance (HasPersistBackend $(return b), PersistQueryRead $(return b)) => PersistQueryRead $(return s)
                deriving via (Compatible $(return b) $(return s)) instance (HasPersistBackend $(return b), PersistUniqueRead $(return b)) => PersistUniqueRead $(return s)
                deriving via (Compatible $(return b) $(return s)) instance (HasPersistBackend $(return b), PersistStoreWrite $(return b)) => PersistStoreWrite $(return s)
                deriving via (Compatible $(return b) $(return s)) instance (HasPersistBackend $(return b), PersistQueryWrite $(return b)) => PersistQueryWrite $(return s)
                deriving via (Compatible $(return b) $(return s)) instance (HasPersistBackend $(return b), PersistUniqueWrite $(return b)) => PersistUniqueWrite $(return s)
            |]

-- | Gives a bunch of useful instance declarations for a backend key based on
-- its compatibility with another backend & key, using 'Compatible'.
--
-- The argument should be a type of the form @ forall v1 ... vn. Compatible b s @
-- (Quantification is optional, but supported because TH won't let you have
-- unbound type variables in a type splice). The instance is produced for
-- @'BackendKey' s@ based on the instance defined for @'BackendKey' b@, which
-- is constrained in the instance head to exist.
--
-- @v1 ... vn@ are implicitly quantified in the instance, which is derived via
-- @'BackendKey' ('Compatible' b s)@.
--
-- @since 2.12
makeCompatibleKeyInstances :: Q Type -> Q [Dec]
makeCompatibleKeyInstances compatibleType = do
    (b, s) <- compatibleType >>= \case
        ForallT _ _ (AppT (AppT (ConT conTName) b) s) ->
            if conTName == ''Compatible
                then pure (b, s)
                else fail $
                            "Cannot make `deriving via` instances if the argument is " <>
                            "not of the form `forall v1 ... vn. Compatible sub sup`"
        AppT (AppT (ConT conTName) b) s ->
            if conTName == ''Compatible
                then pure (b, s)
                else fail $
                            "Cannot make `deriving via` instances if the argument is " <>
                            "not of the form `Compatible sub sup`"
        _ -> fail $
                    "Cannot make `deriving via` instances if the argument is " <>
                    "not of the form `Compatible sub sup`"

    [d|
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), Show (BackendKey $(return b))) => Show (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), Read (BackendKey $(return b))) => Read (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), Eq (BackendKey $(return b))) => Eq (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), Ord (BackendKey $(return b))) => Ord (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), Num (BackendKey $(return b))) => Num (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), Integral (BackendKey $(return b))) => Integral (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), PersistField (BackendKey $(return b))) => PersistField (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), PersistFieldSql (BackendKey $(return b))) => PersistFieldSql (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), Real (BackendKey $(return b))) => Real (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), Enum (BackendKey $(return b))) => Enum (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), Bounded (BackendKey $(return b))) => Bounded (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), ToJSON (BackendKey $(return b))) => ToJSON (BackendKey $(return s))
            deriving via (BackendKey (Compatible $(return b) $(return s))) instance (PersistCore $(return b), PersistCore $(return s), FromJSON (BackendKey $(return b))) => FromJSON (BackendKey $(return s))
        |]
