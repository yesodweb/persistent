{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Persist.QuasiSpec where

import Prelude hiding (lines)

import Control.Exception
import Data.List hiding (lines)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as Map
import qualified Data.Text as T
import Database.Persist.EntityDef.Internal
import Database.Persist.Quasi
import Database.Persist.Quasi.Internal
import Database.Persist.Types
import Test.Hspec
import Test.QuickCheck
import Text.Shakespeare.Text (st, sbt)

defs :: T.Text -> [UnboundEntityDef]
defs t = parse lowerCaseSettings [(Nothing, t)]

defsSnake :: T.Text -> [UnboundEntityDef]
defsSnake t = parse (setPsUseSnakeCaseForeignKeys lowerCaseSettings) [(Nothing, t)]

parseLines :: T.Text -> NonEmpty Line
parseLines txt =
    fromMaybe (error "parseLines failed to parse any lines") $ do
        NEL.nonEmpty (mapMaybe parseLine (T.lines txt))

expectLength :: MonadFail m => Int -> T.Text -> [a] -> m [a]
expectLength n description els =
    if length els == n
       then pure els
       else fail $ "Expected there to be " <> show n <> " " <> T.unpack description <> " but there were " <> show (length els)

spec :: Spec
spec = describe "Quasi" $ do
    describe "takeColsEx" $ do
        let subject = takeColsEx upperCaseSettings
        it "fails on a single word" $ do
            subject ["asdf"]
                `shouldBe`
                    Nothing
        it "errors on invalid input" $ do
            evaluate (subject ["name", "int"])
              `shouldErrorWithMessage` "Invalid field type \"int\" PSFail \"int\""
        it "works if it has a name and a type" $ do
            subject ["asdf", "Int"]
                `shouldBe`
                    Just UnboundFieldDef
                        { unboundFieldNameHS = FieldNameHS "asdf"
                        , unboundFieldNameDB = FieldNameDB "asdf"
                        , unboundFieldType = FTTypeCon Nothing "Int"
                        , unboundFieldAttrs = []
                        , unboundFieldStrict = True
                        , unboundFieldCascade = noCascade
                        , unboundFieldComments = Nothing
                        , unboundFieldGenerated = Nothing
                        }
        it "works if it has a name, type, and cascade" $ do
            subject ["asdf", "Int", "OnDeleteCascade", "OnUpdateCascade"]
                `shouldBe`
                    Just UnboundFieldDef
                        { unboundFieldNameHS = FieldNameHS "asdf"
                        , unboundFieldNameDB = FieldNameDB "asdf"
                        , unboundFieldType = FTTypeCon Nothing "Int"
                        , unboundFieldAttrs = []
                        , unboundFieldStrict = True
                        , unboundFieldCascade = FieldCascade (Just Cascade) (Just Cascade)
                        , unboundFieldComments = Nothing
                        , unboundFieldGenerated = Nothing
                        }
        it "never tries to make a refernece" $ do
            subject ["asdf", "UserId", "OnDeleteCascade"]
                `shouldBe`
                    Just UnboundFieldDef
                        { unboundFieldNameHS = FieldNameHS "asdf"
                        , unboundFieldNameDB = FieldNameDB "asdf"
                        , unboundFieldType = FTTypeCon Nothing "UserId"
                        , unboundFieldAttrs = []
                        , unboundFieldStrict = True
                        , unboundFieldCascade = FieldCascade Nothing (Just Cascade)
                        , unboundFieldComments = Nothing
                        , unboundFieldGenerated = Nothing
                        }

    describe "parseLine" $ do
        it "returns nothing when line is just whitespace" $
            parseLine "         " `shouldBe` Nothing

        it "handles normal words" $
            parseLine " foo   bar  baz" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 1
                        , lineTokens =
                            [ "foo"
                            , "bar"
                            , "baz"
                            ]
                        , lineComment = Nothing
                        }
                    )

        it "handles numbers" $
            parseLine "  one (Finite 1)" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ "one"
                            , "Finite 1"
                            ]
                        , lineComment = Nothing
                        }
                    )

        it "handles quotes" $
            parseLine "  \"foo bar\"  \"baz\"" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ "foo bar"
                            , "baz"
                            ]
                        , lineComment = Nothing
                        }
                    )

        it "should error if quotes are unterminated" $ do
            evaluate (parseLine "    \"foo bar")
                `shouldErrorWithMessage`
                    "Unterminated quoted string starting with foo bar"

        it "handles quotes mid-token" $
            parseLine "  x=\"foo bar\"  \"baz\"" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ "x=foo bar"
                            , "baz"
                            ]
                        , lineComment = Nothing
                        }
                    )

        it "handles escaped quote mid-token" $
            parseLine "  x=\\\"foo bar\"  \"baz\"" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ "x=\\\"foo"
                            , "bar\""
                            , "baz"
                            ]
                        , lineComment = Nothing
                        }
                    )

        it "handles unnested parantheses" $
            parseLine "  (foo bar)  (baz)" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ "foo bar"
                            , "baz"
                            ]
                        , lineComment = Nothing
                        }
                    )

        it "handles unnested parantheses mid-token" $
            parseLine "  x=(foo bar)  (baz)" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ "x=foo bar"
                            , "baz"
                            ]
                        , lineComment = Nothing
                        }
                    )

        it "handles nested parantheses" $
            parseLine "  (foo (bar))  (baz)" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ "foo (bar)"
                            , "baz"
                            ]
                        , lineComment = Nothing
                        }
                    )

        it "escaping" $
            parseLine "  (foo \\(bar)  y=\"baz\\\"\"" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 2
                        , lineTokens =
                            [ "foo (bar"
                            , "y=baz\""
                            ]
                        , lineComment = Nothing
                        }
                    )

        it "mid-token quote in later token" $
            parseLine "foo bar baz=(bin\")" `shouldBe`
                Just
                    ( Line
                        { lineIndent = 0
                        , lineTokens =
                            [ "foo"
                            , "bar"
                            , "baz=bin\""
                            ]
                        , lineComment = Nothing
                        }
                    )

        describe "comments" $ do
            it "recognizes one line" $ do
                parseLine "-- | this is a comment" `shouldBe`
                    Just
                        ( Line
                            { lineIndent = 0
                            , lineTokens = []
                            , lineComment = Just (Comment "-- |" "this is a comment")
                            }
                        )

            it "recognizes empty line" $ do
                parseLine "-- |" `shouldBe`
                    Just
                        ( Line
                            { lineIndent = 0
                            , lineTokens = []
                            , lineComment = Just (Comment "-- |" "")
                            }
                        )

            it "works if comment is indented" $ do
                parseLine "  -- | comment" `shouldBe`
                    Just
                        ( Line
                            { lineIndent = 2
                            , lineTokens = []
                            , lineComment = Just (Comment "-- |" "comment")
                            }
                        )

    describe "parse" $ do
        let subject =
                [st|
Bicycle -- | this is a bike
    brand String -- | the brand of the bike
    ExtraBike
        foo bar  -- | this is a foo bar
        baz
    deriving Eq
-- | This is a Car
Car
    -- | the make of the Car
    make String
    -- | the model of the Car
    model String
    UniqueModel model
    deriving Eq Show
+Vehicle
    bicycle BicycleId -- | the bike reference
    car CarId         -- | the car reference

                    |]
        let [bicycle, car, vehicle] = defs subject

        it "should parse the `entityHaskell` field" $ do
            getUnboundEntityNameHS bicycle `shouldBe` EntityNameHS "Bicycle"
            getUnboundEntityNameHS car `shouldBe` EntityNameHS "Car"
            getUnboundEntityNameHS vehicle `shouldBe` EntityNameHS "Vehicle"

        it "should parse the `entityDB` field" $ do
            entityDB (unboundEntityDef bicycle) `shouldBe` EntityNameDB "bicycle"
            entityDB (unboundEntityDef car) `shouldBe` EntityNameDB "car"
            entityDB (unboundEntityDef vehicle) `shouldBe` EntityNameDB "vehicle"

        it "should parse the `entityAttrs` field" $ do
            entityAttrs (unboundEntityDef bicycle) `shouldBe` []
            entityAttrs (unboundEntityDef car) `shouldBe` []
            entityAttrs (unboundEntityDef vehicle) `shouldBe` []

        it "should parse the `unboundEntityFields` field" $ do
            let simplifyField field =
                    (unboundFieldNameHS field, unboundFieldNameDB field, unboundFieldComments field)
            (simplifyField <$> unboundEntityFields bicycle) `shouldBe`
                [ (FieldNameHS "brand", FieldNameDB "brand", Just "the brand of the bike\n")
                ]
            (simplifyField <$> unboundEntityFields car) `shouldBe`
                [ (FieldNameHS "make", FieldNameDB "make", Just "the make of the Car\n")
                , (FieldNameHS "model", FieldNameDB "model", Just "the model of the Car\n")
                ]
            (simplifyField <$> unboundEntityFields vehicle) `shouldBe`
                [ (FieldNameHS "bicycle", FieldNameDB "bicycle", Just "the bike reference\n")
                , (FieldNameHS "car", FieldNameDB "car", Just "the car reference\n")
                ]

        it "should parse the `entityUniques` field" $ do
            let simplifyUnique unique =
                    (uniqueHaskell unique, uniqueFields unique)
            (simplifyUnique <$> entityUniques (unboundEntityDef bicycle)) `shouldBe` []
            (simplifyUnique <$> entityUniques (unboundEntityDef car)) `shouldBe`
                [ (ConstraintNameHS "UniqueModel", [(FieldNameHS "model", FieldNameDB "model")])
                ]
            (simplifyUnique <$> entityUniques (unboundEntityDef vehicle)) `shouldBe` []

        it "should parse the `entityForeigns` field" $ do
            let [user, notification] = defs [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text

    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst emailSecond
|]
            unboundForeignDefs user `shouldBe` []
            map unboundForeignDef (unboundForeignDefs notification) `shouldBe`
                [ ForeignDef
                    { foreignRefTableHaskell = EntityNameHS "User"
                    , foreignRefTableDBName = EntityNameDB "user"
                    , foreignConstraintNameHaskell = ConstraintNameHS "fk_noti_user"
                    , foreignConstraintNameDBName = ConstraintNameDB "notificationfk_noti_user"
                    , foreignFieldCascade = FieldCascade Nothing Nothing
                    , foreignFields =
                        []
                        -- the foreign fields are not set yet in an unbound
                        -- entity def
                    , foreignAttrs = []
                    , foreignNullable = False
                    , foreignToPrimary = False
                    }
                ]

        it "should parse the `entityDerives` field" $ do
            entityDerives (unboundEntityDef bicycle) `shouldBe` ["Eq"]
            entityDerives (unboundEntityDef car) `shouldBe` ["Eq", "Show"]
            entityDerives (unboundEntityDef vehicle) `shouldBe` []

        it "should parse the `entityEntities` field" $ do
            entityExtra (unboundEntityDef bicycle) `shouldBe` Map.singleton "ExtraBike" [["foo", "bar"], ["baz"]]
            entityExtra (unboundEntityDef car) `shouldBe` mempty
            entityExtra (unboundEntityDef vehicle) `shouldBe` mempty

        it "should parse the `entitySum` field" $ do
            entitySum (unboundEntityDef bicycle) `shouldBe` False
            entitySum (unboundEntityDef car) `shouldBe` False
            entitySum (unboundEntityDef vehicle) `shouldBe` True

        it "should parse the `entityComments` field" $ do
            entityComments (unboundEntityDef bicycle) `shouldBe` Just "this is a bike\n"
            entityComments (unboundEntityDef car) `shouldBe` Just "This is a Car\n"
            entityComments (unboundEntityDef vehicle) `shouldBe` Nothing

        it "should error on malformed input, unterminated parens" $ do
            let definitions = [st|
User
    name Text
    age  (Maybe Int
|]
            let [user] = defs definitions
            evaluate (unboundEntityDef user)
                `shouldErrorWithMessage`
                    "Unterminated parens string starting with Maybe Int"

        it "errors on duplicate cascade update declarations" $ do
            let definitions = [st|
User
    age  Int OnUpdateCascade OnUpdateCascade
|]
            let [user] = defs definitions
            mapM (evaluate . unboundFieldCascade) (unboundEntityFields user)
                `shouldErrorWithMessage`
                    "found more than one OnUpdate action, tokens: [\"OnUpdateCascade\",\"OnUpdateCascade\"]"

        it "errors on duplicate cascade delete declarations" $ do
            let definitions = [st|
User
    age  Int OnDeleteCascade OnDeleteCascade
|]
            let [user] = defs definitions
            mapM (evaluate . unboundFieldCascade) (unboundEntityFields user)
                `shouldErrorWithMessage`
                    "found more than one OnDelete action, tokens: [\"OnDeleteCascade\",\"OnDeleteCascade\"]"

        describe "custom Id column" $ do
            it "parses custom Id column" $ do
                let definitions = [st|
User
    Id   Text
    name Text
    age  Int
|]
                let [user] = defs definitions
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"
                entityDB (unboundEntityDef user) `shouldBe` EntityNameDB "user"
                let idFields = NEL.toList (entitiesPrimary (unboundEntityDef user))
                (fieldHaskell <$> idFields) `shouldBe` [FieldNameHS "Id"]
                (fieldDB <$> idFields) `shouldBe` [FieldNameDB "id"]
                (fieldType <$> idFields) `shouldBe` [FTTypeCon Nothing "Text"]
                (unboundFieldNameHS <$> unboundEntityFields user) `shouldBe`
                    [ FieldNameHS "name"
                    , FieldNameHS "age"
                    ]

            it "errors on duplicate custom Id column" $ do
                let definitions = [st|
User
    Id   Text
    Id   Text
    name Text
    age  Int
|]
                let [user] = defs definitions
                    errMsg = [st|expected only one Id declaration per entity|]
                evaluate (unboundEntityDef user) `shouldErrorWithMessage`
                    (T.unpack errMsg)

        describe "primary declaration" $ do
            it "parses Primary declaration" $ do
                let definitions = [st|
User
    ref Text
    name Text
    age  Int
    Primary ref
|]
                let [user] = defs definitions
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"
                entityDB (unboundEntityDef user) `shouldBe` EntityNameDB "user"
                let idFields = NEL.toList (entitiesPrimary (unboundEntityDef user))
                (fieldHaskell <$> idFields) `shouldBe` [FieldNameHS "Id"]
                (fieldDB <$> idFields) `shouldBe` [FieldNameDB "id"]
                (fieldType <$> idFields) `shouldBe` [FTTypeCon Nothing "UserId"]
                (unboundFieldNameHS <$> unboundEntityFields user) `shouldBe`
                    [ FieldNameHS "ref"
                    , FieldNameHS "name"
                    , FieldNameHS "age"
                    ]
                entityUniques (unboundEntityDef user) `shouldBe`
                    [ UniqueDef
                        { uniqueHaskell =
                            ConstraintNameHS "UserPrimaryKey"
                        , uniqueDBName =
                            ConstraintNameDB "primary_key"
                        , uniqueFields =
                            pure (FieldNameHS "ref", FieldNameDB "ref")
                        , uniqueAttrs =
                            []
                        }
                    ]

            it "errors on duplicate custom Primary declaration" $ do
                let definitions = [st|
User
    ref Text
    name Text
    age  Int
    Primary ref
    Primary name
|]
                let [user] = defs definitions
                    errMsg = "expected only one Primary declaration per entity"
                evaluate (unboundEntityDef user) `shouldErrorWithMessage`
                    errMsg

            it "errors on conflicting Primary/Id declarations" $ do
                let definitions = [st|
User
    Id Text
    ref Text
    name Text
    age  Int
    Primary ref
|]
                let [user] = defs definitions
                    errMsg = [st|Specified both an ID field and a Primary field|]
                evaluate (unboundEntityDef user) `shouldErrorWithMessage`
                    (T.unpack errMsg)

            it "triggers error on invalid declaration" $ do
                let definitions = [st|
User
    age Text
    Primary ref
|]
                let [user] = defs definitions
                case unboundPrimarySpec user of
                    NaturalKey ucd -> do
                        evaluate (NEL.head $ unboundCompositeCols ucd) `shouldErrorWithMessage`
                            "Unknown column in primary key constraint: \"ref\""
                    _ ->
                        error "Expected NaturalKey, failing"

        describe "entity unique constraints" $ do
            it "triggers error if declared field does not exist" $ do
                let definitions = [st|
User
    name            Text
    emailFirst      Text

    UniqueEmail emailFirst emailSecond
|]
                let [user] = defs definitions
                    uniques = entityUniques (unboundEntityDef user)
                    [dbNames] = fmap snd . uniqueFields <$> uniques
                    errMsg = unwords
                        [ "Unknown column in \"UniqueEmail\" constraint: \"emailSecond\""
                        , "possible fields: [\"name\",\"emailFirst\"]"
                        ]
                evaluate (head (NEL.tail dbNames)) `shouldErrorWithMessage`
                    errMsg

            it "triggers error if no valid constraint name provided" $ do
                let definitions = [st|
User
    age Text
    Unique some
|]
                let [user] = defs definitions
                evaluate (unboundPrimarySpec user) `shouldErrorWithMessage`
                    "invalid unique constraint on table[\"User\"] expecting an uppercase constraint name xs=[\"some\"]"

        describe "foreign keys" $ do
            let validDefinitions = [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text

    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst emailSecond
|]

            it "should allow you to modify the FK name via provided function" $ do
                let
                    flippedFK (EntityNameHS entName) (ConstraintNameHS conName) =
                        conName <> entName
                    [_user, notification] =
                        parse (setPsToFKName flippedFK lowerCaseSettings) [(Nothing, validDefinitions)]
                    [notificationForeignDef] =
                        unboundForeignDef <$> unboundForeignDefs notification
                foreignConstraintNameDBName notificationForeignDef
                    `shouldBe`
                        ConstraintNameDB "fk_noti_user_notification"

            it "should error when insufficient params provided" $ do
                let definitions = [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text
    Foreign User
|]
                let [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage`
                        "invalid foreign key constraint on table[\"Notification\"] expecting a lower case constraint name or a cascading action xs=[]"

            it "should error when foreign fields not provided" $ do
                let definitions = [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text
    Foreign User fk_noti_user
|]
                let [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage`
                        "No fields on foreign reference."

            it "should error when number of parent and foreign fields differ" $ do
                let definitions = [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text
    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst
|]
                let [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage`
                        "invalid foreign key constraint on table[\"Notification\"] Found 2 foreign fields but 1 parent fields"

            it "should throw error when there is more than one delete cascade on the declaration" $ do
                let definitions = [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text
    Foreign User OnDeleteCascade OnDeleteCascade
|]
                let [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage`
                        "invalid foreign key constraint on table[\"Notification\"] found more than one OnDelete actions"

            it "should throw error when there is more than one update cascade on the declaration" $ do
                let definitions = [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text
    Foreign User OnUpdateCascade OnUpdateCascade
|]
                let [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage`
                        "invalid foreign key constraint on table[\"Notification\"] found more than one OnUpdate actions"

            it "should allow you to enable snake cased foriegn keys via a preset configuration function" $ do
                let [_user, notification] =
                        defsSnake validDefinitions
                    [notificationForeignDef] =
                        unboundForeignDef <$> unboundForeignDefs notification
                foreignConstraintNameDBName notificationForeignDef
                    `shouldBe`
                        ConstraintNameDB "notification_fk_noti_user"

        describe "ticked types" $ do
            it "should be able to parse ticked types" $ do
                let simplifyField field =
                        (unboundFieldNameHS field, unboundFieldType field)
                let tickedDefinition = [st|
CustomerTransfer
    customerId CustomerId
    moneyAmount (MoneyAmount 'Customer 'Debit)
    currencyCode CurrencyCode
    uuid TransferUuid
|]
                let [customerTransfer] = defs tickedDefinition
                let expectedType =
                        FTTypeCon Nothing "MoneyAmount" `FTApp` FTTypePromoted "Customer" `FTApp` FTTypePromoted "Debit"

                (simplifyField <$> unboundEntityFields customerTransfer) `shouldBe`
                    [ (FieldNameHS "customerId", FTTypeCon Nothing "CustomerId")
                    , (FieldNameHS "moneyAmount", expectedType)
                    , (FieldNameHS "currencyCode", FTTypeCon Nothing "CurrencyCode")
                    , (FieldNameHS "uuid", FTTypeCon Nothing "TransferUuid")
                    ]

        describe "type literals" $ do
            it "should be able to parse type literals" $ do
                let simplifyField field =
                        (unboundFieldNameHS field, unboundFieldType field)
                let tickedDefinition = [st|
WithFinite
    one    (Finite 1)
    twenty (Labelled "twenty")
|]
                let [withFinite] = defs tickedDefinition

                (simplifyField <$> unboundEntityFields withFinite) `shouldBe`
                    [ (FieldNameHS "one", FTApp (FTTypeCon Nothing "Finite") (FTLit (IntTypeLit 1)))
                    , (FieldNameHS "twenty", FTApp (FTTypeCon Nothing "Labelled") (FTLit (TextTypeLit "twenty")))
                    ]

    describe "parseFieldType" $ do
        it "simple types" $
            parseFieldType "FooBar" `shouldBe` Right (FTTypeCon Nothing "FooBar")
        it "module types" $
            parseFieldType "Data.Map.FooBar" `shouldBe` Right (FTTypeCon (Just "Data.Map") "FooBar")
        it "application" $
            parseFieldType "Foo Bar" `shouldBe` Right (
                FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
        it "application multiple" $
            parseFieldType "Foo Bar Baz" `shouldBe` Right (
                (FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
                `FTApp` FTTypeCon Nothing "Baz"
                )
        it "parens" $ do
            let foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo (Bar Baz)" `shouldBe` Right (
                foo `FTApp` (bar `FTApp` baz))
        it "lists" $ do
            let foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                bars = FTList bar
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo [Bar] Baz" `shouldBe` Right (
                foo `FTApp` bars `FTApp` baz)
        it "numeric type literals" $ do
            let expected = FTApp (FTTypeCon Nothing "Finite") (FTLit (IntTypeLit 1))
            parseFieldType "Finite 1" `shouldBe` Right expected
        it "string type literals" $ do
            let expected = FTApp (FTTypeCon Nothing "Labelled") (FTLit (TextTypeLit "twenty"))
            parseFieldType "Labelled \"twenty\"" `shouldBe` Right expected
        it "nested list / parens (list inside parens)" $ do
            let maybeCon = FTTypeCon Nothing "Maybe"
                int = FTTypeCon Nothing "Int"
            parseFieldType "Maybe (Maybe [Int])" `shouldBe` Right
                (maybeCon `FTApp` (maybeCon `FTApp` FTList int))
        it "nested list / parens (parens inside list)" $ do
            let maybeCon = FTTypeCon Nothing "Maybe"
                int = FTTypeCon Nothing "Int"
            parseFieldType "[Maybe (Maybe Int)]" `shouldBe` Right
                (FTList (maybeCon `FTApp` (maybeCon `FTApp` int)))
        it "fails on lowercase starts" $ do
            parseFieldType "nothanks" `shouldBe` Left "PSFail \"nothanks\""

    describe "#1175 empty entity" $ do
        let subject =
                [st|
Foo
    name String
    age Int

EmptyEntity

Bar
    name String

Baz
    a Int
    b String
    c FooId
                    |]

        let preparsed = parseLines subject

        it "preparse works" $ do
            length preparsed `shouldBe` 10

        it "parseEntityDefs works" $ do
            let simplifyParsedFieldDef pfa =
                    (parsedFieldDefTokens pfa, parsedFieldDefComments pfa)

            let parsedDefs = parseEntityDefs Nothing preparsed

            [fooDef, emptyEntityDef, barDef, bazDef] <- expectLength 4 "parsed definitions" parsedDefs

            parsedEntityDefEntityName fooDef `shouldBe` EntityNameHS "Foo"
            parsedEntityDefComments fooDef `shouldBe` []

            [fooName, fooAge] <- expectLength 2 "Foo fields" (parsedEntityDefFieldAttributes fooDef)
            simplifyParsedFieldDef fooName `shouldBe` (["name", "String"], [])
            simplifyParsedFieldDef fooAge `shouldBe` (["age", "Int"], [])

            parsedEntityDefEntityName emptyEntityDef `shouldBe` EntityNameHS "EmptyEntity"
            parsedEntityDefComments emptyEntityDef `shouldBe` []

            [] <- expectLength 0 "EmptyEntity fields" (parsedEntityDefFieldAttributes emptyEntityDef)

            parsedEntityDefEntityName barDef `shouldBe` EntityNameHS "Bar"
            parsedEntityDefComments barDef `shouldBe` []

            [barName] <- expectLength 1 "Bar fields" (parsedEntityDefFieldAttributes barDef)
            simplifyParsedFieldDef barName `shouldBe` (["name", "String"], [])

            parsedEntityDefEntityName bazDef `shouldBe` EntityNameHS "Baz"
            parsedEntityDefComments bazDef `shouldBe` []

            [bazA, bazB, bazC] <- expectLength 3 "Baz fields" (parsedEntityDefFieldAttributes bazDef)
            simplifyParsedFieldDef bazA `shouldBe` (["a", "Int"], [])
            simplifyParsedFieldDef bazB `shouldBe` (["b", "String"], [])
            simplifyParsedFieldDef bazC `shouldBe` (["c", "FooId"], [])

        it "parse works" $ do
            let test name'fieldCount parsedList = do
                    case (name'fieldCount, parsedList) of
                        ([], []) ->
                            pure ()
                        ((name, fieldCount) : _, []) ->
                            expectationFailure
                                $ "Expected an entity with name "
                                <> name
                                <> " and " <> show fieldCount <> " fields"
                                <> ", but the list was empty..."

                        ((name, fieldCount) : ys, (x : xs)) -> do
                            let
                                UnboundEntityDef {..} =
                                    x
                            (unEntityNameHS (getUnboundEntityNameHS x), length unboundEntityFields)
                                `shouldBe`
                                    (T.pack name, fieldCount)
                            test ys xs
                        ([], _:_) ->
                            expectationFailure
                                "more entities parsed than expected"

                result =
                    defs subject
            length result `shouldBe` 4

            test
                [ ("Foo", 2)
                , ("EmptyEntity", 0)
                , ("Bar", 1)
                , ("Baz", 3)
                ]
                result

    describe "preparse" $ do
        it "recognizes entity" $ do
            let expected =
                    Line { lineIndent = 0, lineTokens = ["Person"], lineComment = Nothing } :|
                    [ Line { lineIndent = 2, lineTokens = ["name", "String"], lineComment = Nothing }
                    , Line { lineIndent = 2, lineTokens = ["age", "Int"], lineComment = Nothing }
                    ]

            preparse "Person\n  name String\n  age Int" `shouldBe` Just (3, expected)

        it "recognizes comments" $ do
            let text = "Foo\n  x X\n-- | Hello\nBar\n name String"
            let expected =
                    Line { lineIndent = 0, lineTokens = ["Foo"], lineComment = Nothing } :|
                    [ Line { lineIndent = 2, lineTokens = ["x", "X"], lineComment = Nothing }
                    , Line { lineIndent = 0, lineTokens = [], lineComment = Just (Comment "-- |" "Hello") }
                    , Line { lineIndent = 0, lineTokens = ["Bar"], lineComment = Nothing }
                    , Line { lineIndent = 1, lineTokens = ["name", "String"], lineComment = Nothing }
                    ]
            preparse text `shouldBe` Just (5, expected)

        it "preparse indented" $ do
            let t = T.unlines
                    [ "  Foo"
                    , "    x X"
                    , "  -- | Comment"
                    , "  -- hidden comment"
                    , "  Bar"
                    , "    name String"
                    ]
                expected =
                    Line { lineIndent = 2, lineTokens = ["Foo"], lineComment = Nothing } :|
                    [ Line { lineIndent = 4, lineTokens = ["x", "X"], lineComment = Nothing }
                    , Line { lineIndent = 2, lineTokens = [], lineComment = Just (Comment "-- |" "Comment") }
                    , Line { lineIndent = 2, lineTokens = [], lineComment = Just (Comment "--" "hidden comment") }
                    , Line { lineIndent = 2, lineTokens = ["Bar"], lineComment = Nothing }
                    , Line { lineIndent = 4, lineTokens = ["name", "String"], lineComment = Nothing }
                    ]
            preparse t `shouldBe` Just (6, expected)

        it "preparse extra blocks" $ do
            let t = T.unlines
                    [ "LowerCaseTable"
                    , "  name String"
                    , "  ExtraBlock"
                    , "    foo bar"
                    , "    baz"
                    , "  ExtraBlock2"
                    , "    something"
                    ]
                expected =
                    Line { lineIndent = 0, lineTokens = ["LowerCaseTable"], lineComment = Nothing } :|
                    [ Line { lineIndent = 2, lineTokens = ["name", "String"], lineComment = Nothing }
                    , Line { lineIndent = 2, lineTokens = ["ExtraBlock"], lineComment = Nothing }
                    , Line { lineIndent = 4, lineTokens = ["foo", "bar"], lineComment = Nothing }
                    , Line { lineIndent = 4, lineTokens = ["baz"], lineComment = Nothing }
                    , Line { lineIndent = 2, lineTokens = ["ExtraBlock2"], lineComment = Nothing }
                    , Line { lineIndent = 4, lineTokens = ["something"], lineComment = Nothing }
                    ]
            preparse t `shouldBe` Just (7, expected)

        it "field comments" $ do
            let text = T.unlines
                    [ "-- | Model"
                    , "Foo"
                    , "  -- | Field"
                    , "  name String"
                    ]
                expected =
                    Line { lineIndent = 0, lineTokens = [], lineComment = Just (Comment "-- |" "Model") } :|
                    [ Line { lineIndent = 0, lineTokens = ["Foo"], lineComment = Nothing }
                    , Line { lineIndent = 2, lineTokens = [], lineComment = Just (Comment "-- |" "Field") }
                    , Line { lineIndent = 2, lineTokens = ["name", "String"], lineComment = Nothing }
                    ]
            preparse text `shouldBe` Just (4, expected)

    describe "parseEntityDefs" $ do
        let simplifyParsedFieldDef pfa =
                (parsedFieldDefTokens pfa, parsedFieldDefComments pfa)

        it "parses an entity with one field" $ do
            let simpleEntity =
                    parseLines
                        [sbt|Foo
                            |  name String
                            |]
            let [fooDef] = parseEntityDefs Nothing simpleEntity
            parsedEntityDefComments fooDef `shouldBe` []
            parsedEntityDefEntityName fooDef `shouldBe` EntityNameHS "Foo"
            let [fooName] = parsedEntityDefFieldAttributes fooDef
            simplifyParsedFieldDef fooName `shouldBe` (["name", "String"], [])

        let foo = parseLines
                      [sbt|-- | comment
                          |Foo
                          |  name String
                          |]

        it "parses an entity with a comment" $ do
            let [fooDef] = parseEntityDefs Nothing foo
            parsedEntityDefComments fooDef `shouldBe` [Comment "-- |" "comment"]
            parsedEntityDefEntityName fooDef `shouldBe` EntityNameHS "Foo"
            let [fooName] = parsedEntityDefFieldAttributes fooDef
            simplifyParsedFieldDef fooName `shouldBe` (["name", "String"], [])

        it "works with field comments" $ do
            let text = parseLines
                  [sbt|-- | Model
                      |Foo
                      |  -- | Field
                      |  name String
                      |]
            let [fooDef] = parseEntityDefs Nothing text
            parsedEntityDefComments fooDef `shouldBe` [Comment "-- |" "Model"]
            parsedEntityDefEntityName fooDef `shouldBe` EntityNameHS "Foo"
            let simpleFields = simplifyParsedFieldDef <$> parsedEntityDefFieldAttributes fooDef
            simpleFields `shouldBe`
                [ (["name", "String"], [Comment "-- |" "Field"])
                ]

        let bar = parseLines
                      [sbt|Bar sql=bars
                          | age Int
                          |]

        it "works when used for multiple definitions" $ do
            let [fooDef, barDef] = parseEntityDefs Nothing (foo <> bar)

            parsedEntityDefComments fooDef `shouldBe` [Comment "-- |" "comment"]
            parsedEntityDefEntityName fooDef `shouldBe` EntityNameHS "Foo"
            let simpleFooFields = simplifyParsedFieldDef <$> parsedEntityDefFieldAttributes fooDef
            simpleFooFields `shouldBe`
                [ (["name", "String"], [])
                ]

            parsedEntityDefComments barDef `shouldBe` []
            parsedEntityDefEntityName barDef `shouldBe` EntityNameHS "Bar"
            let simpleBarFields = simplifyParsedFieldDef <$> parsedEntityDefFieldAttributes barDef
            simpleBarFields `shouldBe`
                [ (["age", "Int"], [])
                ]

        it "can parse one indented entity" $ do
            let indentedLines = parseLines
                    [sbt|    CompanyUser
                        |        name Text
                        |]
            let [companyUserDef] = parseEntityDefs Nothing indentedLines
            parsedEntityDefEntityName companyUserDef `shouldBe` EntityNameHS "CompanyUser"
            let simpleFields = simplifyParsedFieldDef <$> parsedEntityDefFieldAttributes companyUserDef
            simpleFields `shouldBe`
                [ (["name", "Text"], [])
                ]

        it "can parse indented entities" $ do
            let companyUserLines = parseLines
                    [sbt|   CompanyUser
                        |      companyName Text
                        |      userName Text
                        |      Primary companyName userName
                        |
                        |   CompanyUser2
                        |      companyName Text
                        |      userName Text
                        |      Primary companyName userName
                        |]
            let [companyUserDef, _] = parseEntityDefs Nothing companyUserLines
            parsedEntityDefComments companyUserDef `shouldBe` []
            parsedEntityDefEntityName companyUserDef `shouldBe` EntityNameHS "CompanyUser"
            let simpleFields = simplifyParsedFieldDef <$> parsedEntityDefFieldAttributes companyUserDef
            simpleFields `shouldBe`
                [ (["companyName", "Text"], [])
                , (["userName", "Text"], [])
                , (["Primary", "companyName", "userName"], [])
                ]

        it "parse entity example - entity with default field" $ do
            let productLines = parseLines
                    [sbt|Product
                        |  name Text
                        |  added UTCTime default=CURRENT_TIMESTAMP
                        |]
            let [productDef] = parseEntityDefs Nothing productLines
            parsedEntityDefComments productDef `shouldBe` []
            parsedEntityDefEntityName productDef `shouldBe` EntityNameHS "Product"
            let simpleFields = simplifyParsedFieldDef <$> parsedEntityDefFieldAttributes productDef
            simpleFields `shouldBe`
                [ (["name", "Text"], [])
                , (["added", "UTCTime", "default=CURRENT_TIMESTAMP"], [])
                ]

        it "parse entity example - entity with extra block 1" $ do
            let productLines = parseLines
                    [sbt|Product
                        |  name Text
                        |    ExtraBlock
                        |    added UTCTime default=CURRENT_TIMESTAMP
                        |]
            let [productDef] = parseEntityDefs Nothing productLines
            parsedEntityDefComments productDef `shouldBe` []
            parsedEntityDefEntityName productDef `shouldBe` EntityNameHS "Product"
            let simpleFields = simplifyParsedFieldDef <$> parsedEntityDefFieldAttributes productDef
            simpleFields `shouldBe`
                [ (["name", "Text"], [])
                , (["added", "UTCTime", "default=CURRENT_TIMESTAMP"], [])
                ]
            let extras = parsedEntityDefExtras productDef
            extras `shouldBe`
                [ ("ExtraBlock", [])
                ]

        it "parse entity example - entity with extra block 2" $ do
            let lowerCaseTableLines = parseLines
                    [sbt|LowerCaseTable
                        |    Id             sql=my_id
                        |    fullName Text
                        |    ExtraBlock
                        |        foo bar
                        |        baz
                        |        bin
                        |    ExtraBlock2
                        |        something
                        |]
            let [lowerCaseTableDef] = parseEntityDefs Nothing lowerCaseTableLines
            parsedEntityDefComments lowerCaseTableDef `shouldBe` []
            parsedEntityDefEntityName lowerCaseTableDef `shouldBe` EntityNameHS "LowerCaseTable"
            let simpleFields = simplifyParsedFieldDef <$> parsedEntityDefFieldAttributes lowerCaseTableDef
            simpleFields `shouldBe`
                [ (["Id", "sql=my_id"], [])
                , (["fullName", "Text"], [])
                ]
            let extras = parsedEntityDefExtras lowerCaseTableDef
            extras `shouldBe`
                [ ("ExtraBlock",
                    [ ["foo", "bar"]
                    , ["baz"]
                    , ["bin"]
                    ])
                , ("ExtraBlock2",
                    [ ["something"]
                    ])
                ]

        it "works with two extra blocks" $ do
            let lowerCaseTableLines = parseLines
                    [sbt|IdTable
                        |    Id Day default=CURRENT_DATE
                        |    name Text
                        |
                        |LowerCaseTable
                        |    Id             sql=my_id
                        |    fullName Text
                        |    ExtraBlock
                        |        foo bar
                        |        baz
                        |        bin
                        |    ExtraBlock2
                        |        something
                        |]
            let [idTableDef, lowerCaseTableDef] = parseEntityDefs Nothing lowerCaseTableLines
            parsedEntityDefComments idTableDef `shouldBe` []
            parsedEntityDefEntityName idTableDef `shouldBe` EntityNameHS "IdTable"
            let simpleFieldsId = simplifyParsedFieldDef <$> parsedEntityDefFieldAttributes idTableDef
            simpleFieldsId `shouldBe`
                [ (["Id", "Day", "default=CURRENT_DATE"], [])
                , (["name", "Text"], [])
                ]
            let idExtras = parsedEntityDefExtras idTableDef
            idExtras `shouldBe` []

            parsedEntityDefComments lowerCaseTableDef `shouldBe` []
            parsedEntityDefEntityName lowerCaseTableDef `shouldBe` EntityNameHS "LowerCaseTable"
            let simpleFields = simplifyParsedFieldDef <$> parsedEntityDefFieldAttributes lowerCaseTableDef
            simpleFields `shouldBe`
                [ (["Id", "sql=my_id"], [])
                , (["fullName", "Text"], [])
                ]
            let extras = parsedEntityDefExtras lowerCaseTableDef
            extras `shouldBe`
                [ ("ExtraBlock",
                    [ ["foo", "bar"]
                    , ["baz"]
                    , ["bin"]
                    ])
                , ("ExtraBlock2",
                    [ ["something"]
                    ])
                ]

    describe "parseLines" $ do
        let lines =
                T.unlines
                    [ "-- | Comment"
                    , "Foo"
                    , "  -- | Field"
                    , "  name String"
                    , "  age  Int"
                    , "  Extra"
                    , "    foo bar"
                    , "    baz"
                    , "  Extra2"
                    , "    something"
                    ]
        let [subject] = defs lines
        it "produces the right name" $ do
            getUnboundEntityNameHS subject `shouldBe` EntityNameHS "Foo"
        describe "unboundEntityFields" $ do
            let fields = unboundEntityFields subject
            it "has the right field names" $ do
                map unboundFieldNameHS fields `shouldMatchList`
                    [ FieldNameHS "name"
                    , FieldNameHS "age"
                    ]
            it "has comments" $ do
                map unboundFieldComments fields `shouldBe`
                    [ Just "Field\n"
                    , Nothing
                    ]
        it "has the comments" $ do
            entityComments (unboundEntityDef subject) `shouldBe`
                Just "Comment\n"
        it "combines extrablocks" $ do
            entityExtra (unboundEntityDef subject) `shouldBe` Map.fromList
                [ ("Extra", [["foo", "bar"], ["baz"]])
                , ("Extra2", [["something"]])
                ]
        describe "works with extra blocks" $ do
            let [_, lowerCaseTable, idTable] =
                    case defs $ T.unlines
                        [ ""
                        , "IdTable"
                        , "    Id Day default=CURRENT_DATE"
                        , "    name Text"
                        , ""
                        , "LowerCaseTable"
                        , "    Id             sql=my_id"
                        , "    fullName Text"
                        , "    ExtraBlock"
                        , "        foo bar"
                        , "        baz"
                        , "        bin"
                        , "    ExtraBlock2"
                        , "        something"
                        , ""
                        , "IdTable"
                        , "    Id Day default=CURRENT_DATE"
                        , "    name Text"
                        , ""
                        ] of
                            [a, b, c] ->
                                [a, b, c] :: [UnboundEntityDef]
                            xs ->
                                error
                                $ "Expected 3 elements in list, got: "
                                <> show (length xs)
                                <> ", list contents: \n\n" <> intercalate "\n" (map show xs)
            describe "idTable" $ do
                let UnboundEntityDef { unboundEntityDef = EntityDef {..}, .. } = idTable
                it "has no extra blocks" $ do
                    entityExtra `shouldBe` mempty
                it "has the right name" $ do
                    entityHaskell `shouldBe` EntityNameHS "IdTable"
                it "has the right fields" $ do
                    map unboundFieldNameHS unboundEntityFields `shouldMatchList`
                        [ FieldNameHS "name"
                        ]
            describe "lowerCaseTable" $ do
                let UnboundEntityDef { unboundEntityDef = EntityDef {..}, ..} = lowerCaseTable
                it "has the right name" $ do
                    entityHaskell `shouldBe` EntityNameHS "LowerCaseTable"
                it "has the right fields" $ do
                    map unboundFieldNameHS unboundEntityFields `shouldMatchList`
                        [ FieldNameHS "fullName"
                        ]
                it "has ExtraBlock" $ do
                    Map.lookup "ExtraBlock" entityExtra
                        `shouldBe` Just
                            [ ["foo", "bar"]
                            , ["baz"]
                            , ["bin"]
                            ]
                it "has ExtraBlock2" $ do
                    Map.lookup "ExtraBlock2" entityExtra
                        `shouldBe` Just
                            [ ["something"]
                            ]

arbitraryWhiteSpaceChar :: Gen Char
arbitraryWhiteSpaceChar =
  oneof $ pure <$> [' ', '\t', '\n', '\r']

shouldErrorWithMessage :: IO a -> String -> Expectation
shouldErrorWithMessage action expectedMsg = do
    res <- try action
    case res of
        Left (ErrorCall msg) ->
            msg `shouldBe` expectedMsg
        _ ->
            expectationFailure "Expected `error` to have been called"
