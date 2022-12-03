{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.TH.PersistWithSpec where

import Control.Monad
import TemplateTestImports
import Database.Persist.TH.PersistWith.Model as Model (IceCream, IceCreamId)
import Language.Haskell.TH as TH

mkPersistWith sqlSettings $(discoverEntities) [persistLowerCase|

BestTopping
    iceCream IceCreamId
    otherCream Model.IceCreamId
    keyCream (Key IceCream)
    qualifiedKeyCream (Key Model.IceCream)
    nullableCream IceCreamId Maybe
    maybeCream (Maybe IceCreamId)
    maybeQualifiedCream (Maybe Model.IceCreamId)
    maybeQualifiedKeyCream (Maybe (Key Model.IceCream))
    maybeKeyCream (Maybe (Key IceCream))

|]

deriving instance Show (EntityField BestTopping a)
deriving instance Eq (EntityField BestTopping a)

data SomeField where
    SomeField :: EntityField BestTopping a -> SomeField

allFields =
    [ SomeField BestToppingIceCream
    , SomeField BestToppingOtherCream
    , SomeField BestToppingKeyCream
    , SomeField BestToppingQualifiedKeyCream
    , SomeField BestToppingMaybeCream
    , SomeField BestToppingNullableCream
    , SomeField BestToppingMaybeQualifiedCream
    , SomeField BestToppingMaybeQualifiedKeyCream
    , SomeField BestToppingMaybeKeyCream
    ]

spec :: Spec
spec = describe "mkPersistWith" $ do
    describe "finds references" $ do
        forM_ allFields $ \(SomeField field) ->
            it (show field) (shouldReferToIceCream field)

shouldReferToIceCream :: EntityField BestTopping a -> IO ()
shouldReferToIceCream field =
    unless (reference == iceCreamRef) $ do
        expectationFailure $ mconcat
            [ "The field '", show field, "' does not have a reference to IceCream.\n"
            , "Got Reference: ", show reference, "\n"
            , "Expected     : ", show iceCreamRef
            ]
  where
    reference =
        fieldReference (persistFieldDef field)
    iceCreamRef =
        ForeignRef (EntityNameHS "IceCream")
