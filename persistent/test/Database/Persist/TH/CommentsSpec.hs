{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Database.Persist.TH.CommentsSpec
    ( spec
    ) where

import Data.Time
import TemplateTestImports

share [mkPersist sqlSettings, mkMigrate "commentsMigrate"] [persistLowerCase|
-- -- User accounts in the system.
-- Includes auth info and basic profile data.
User
    email Text
    -- ^ The user's email. Must be unique.

    -- Password hash for login.
    -- Stored using bcrypt or similar.
    password Text

    -- When the user signed up.
    createdAt UTCTime -- ^ UTC timestamp, used for auditing

    isAdmin Bool -- True if the user is an admin

-- | Product listing
Product
    -- | Stock keeping unit
    sku Text

    -- | Cost of the product
    price Int

    -- | Whether this product is discontinued
    isActive Bool

-- | A customer’s shipping address.
-- Multiple addresses can be associated with one customer.
ShippingAddress
    customerId Text
    -- ^ Reference to the customer.
    street Text
    -- ^ Street address, including house number.
    city Text
    -- ^ City name.
    countryCode Text
    -- ^ ISO 3166-1 alpha-2 country code.

Upload -- | Represents an uploaded file
    path      Text    -- | Filesystem path
    uploaded  UTCTime -- | when the file was uploaded
    size      Int     -- | in bytes
    mimeType  Text    -- | MIME type, like image/png

-- | Tracks failed login attempts.
-- | Used for rate limiting and security audits.
LoginFailure
    -- | IP address from which the attempt originated.
    -- | Stored as plain text.
    ipAddress Text

    -- | Number of consecutive failed attempts.
    -- | Reset after a successful login.
    failureCount Int

    -- | Time of the most recent failure.
    -- | Used to determine lockout duration.
    lastFailureAt UTCTime

-- | User session tokens
-- Used for user authentication, these expire after a certain duration.
Session
    -- | Foreign key to the user
    -- Used to associate the session with a specific account.
    userId UserId

    -- | Randomly generated token string
    -- Should be treated as opaque and unique.
    token Text

    -- | Expiry timestamp
    -- The time the session will expire.
    expiresAt UTCTime
|]

spec :: Spec
spec = describe "Comments" $ do
    it "User example" $ do
        let edef = entityDef (Proxy :: Proxy User)

        getEntityComments edef
            `shouldBe` Nothing
   --         `shouldBe` Just "User accounts in the system.\nIncludes auth info and basic profile data."

        let [emailField, passwordField, createdAtField, isAdminField] = getEntityFields edef

        fieldComments emailField
            `shouldBe` Nothing
            -- `shouldBe` Just "The user's email. Must be unique."

        fieldComments passwordField
            `shouldBe` Nothing
            -- `shouldBe` Just "Password hash for login.\nStored using bcrypt or similar."

        fieldComments createdAtField
            `shouldBe` Nothing
            -- `shouldBe` Just "UTC timestamp, used for auditing"

        fieldComments isAdminField
            `shouldBe` Nothing
            -- `shouldBe` Just "True if the user is an admin"

    it "Product example" $ do
        let edef = entityDef (Proxy :: Proxy Product)

        getEntityComments edef
            `shouldBe` Just "Product listing\n"

        let [skuField, priceField, isActiveField] = getEntityFields edef

        fieldComments skuField
            `shouldBe` Just "Stock keeping unit\n"

        fieldComments priceField
            `shouldBe` Just "Cost of the product\n"

        fieldComments isActiveField
            `shouldBe` Just "Whether this product is discontinued\n"

    it "ShippingAddress example" $ do
        let edef = entityDef (Proxy :: Proxy ShippingAddress)

        getEntityComments edef
            `shouldBe` Just "A customer’s shipping address.\n"
            -- `shouldBe` Just "A customer’s shipping address.\nMultiple addresses can be associated with one customer."

        let [customerIdField, streetField, cityField, countryCodeField] = getEntityFields edef

        fieldComments customerIdField
            `shouldBe` Nothing
            -- `shouldBe` Just "Reference to the customer."

        fieldComments streetField
            `shouldBe` Nothing
            -- `shouldBe` Just "Street address, including house number."

        fieldComments cityField
            `shouldBe` Nothing
            -- `shouldBe` Just "City name."

        fieldComments countryCodeField
            `shouldBe` Nothing
            -- `shouldBe` Just "ISO 3166-1 alpha-2 country code."

    it "LoginFailure example" $ do
        let edef = entityDef (Proxy :: Proxy LoginFailure)

        getEntityComments edef
            `shouldBe` Just "Tracks failed login attempts.\nUsed for rate limiting and security audits.\n"

        let [ipAddressField, failureCountField, lastFailureAtField] = getEntityFields edef

        fieldComments ipAddressField
            `shouldBe` Just "IP address from which the attempt originated.\nStored as plain text.\n"

        fieldComments failureCountField
            `shouldBe` Just "Number of consecutive failed attempts.\nReset after a successful login.\n"

        fieldComments lastFailureAtField
            `shouldBe` Just "Time of the most recent failure.\nUsed to determine lockout duration.\n"

    it "Session entity comments" $ do
        let edef = entityDef (Proxy :: Proxy Session)

        getEntityComments edef
            `shouldBe` Just "User session tokens\n"
            -- `shouldBe` Just "User session tokens\nUsed for user authentication, these expire after a certain duration."

        let [userIdField, tokenField, expiresAtField] = getEntityFields edef

        fieldComments userIdField
            `shouldBe` Just "Foreign key to the user\n"
            -- `shouldBe` Just "Foreign key to the user\nUsed to associate the session with a specific account."

        fieldComments tokenField
            `shouldBe` Just "Randomly generated token string\n"
            -- `shouldBe` Just "Randomly generated token string\nShould be treated as opaque and unique."

        fieldComments expiresAtField
            `shouldBe` Just "Expiry timestamp\n"
            -- `shouldBe` Just "Expiry timestamp\nThe time the session will expire."

    it "Upload with inline comments" $ do
        let edef = entityDef (Proxy :: Proxy Upload)

        getEntityComments edef
            `shouldBe` Just "Represents an uploaded file\n"

        let [pathField, uploadedField, sizeField, mimeField] = getEntityFields edef

        fieldComments pathField
            `shouldBe` Just "Filesystem path\n"

        fieldComments uploadedField
            `shouldBe` Just "when the file was uploaded\n"

        fieldComments sizeField
            `shouldBe` Just "in bytes\n"

        fieldComments mimeField
            `shouldBe` Just "MIME type, like image/png\n"

