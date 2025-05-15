{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Persist.QuasiSpec where

import Prelude hiding (lines)

import Control.Exception
import Data.Bifunctor
import Data.List hiding (lines)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.Text as T
import Database.Persist.EntityDef.Internal
import Database.Persist.Quasi
import Database.Persist.Quasi.Internal
import Database.Persist.Quasi.Internal.ModelParser
import Database.Persist.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Shakespeare.Text (st)
import Text.Megaparsec (errorBundlePretty, runParser, some)

defs :: T.Text -> [UnboundEntityDef]
defs = defsWithSettings lowerCaseSettings

defsSnake :: T.Text -> [UnboundEntityDef]
defsSnake = defsWithSettings $ setPsUseSnakeCaseForeignKeys lowerCaseSettings

defsWithSettings :: PersistSettings -> T.Text -> [UnboundEntityDef]
defsWithSettings ps t = case cpr of
    Right res -> res
    Left errs -> error $ renderErrors errs
  where
    cpr = parse ps [(Nothing, t)]

spec :: Spec
spec = describe "Quasi" $ do
    describe "takeColsEx" $ do
        let
            subject = takeColsEx upperCaseSettings
        it "fails on a single word" $ do
            subject ["asdf"]
                `shouldBe` Nothing
        it "errors on invalid input" $ do
            evaluate (subject ["name", "int"])
                `shouldErrorWithMessage` "Invalid field type \"int\" PSFail \"int\""
        it "works if it has a name and a type" $ do
            subject ["asdf", "Int"]
                `shouldBe` Just
                    UnboundFieldDef
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
                `shouldBe` Just
                    UnboundFieldDef
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
                `shouldBe` Just
                    UnboundFieldDef
                        { unboundFieldNameHS = FieldNameHS "asdf"
                        , unboundFieldNameDB = FieldNameDB "asdf"
                        , unboundFieldType = FTTypeCon Nothing "UserId"
                        , unboundFieldAttrs = []
                        , unboundFieldStrict = True
                        , unboundFieldCascade = FieldCascade Nothing (Just Cascade)
                        , unboundFieldComments = Nothing
                        , unboundFieldGenerated = Nothing
                        }

    describe "tokenization" $ do
        let
            tokenize s = do
                (d, c) <- runConfiguredParser initialExtraState (some anyToken) "" s
                pure d
        it "handles normal words" $
            tokenize "foo   bar  baz"
                `shouldBe` Right
                    ( [ PText "foo"
                      , PText "bar"
                      , PText "baz"
                      ]
                    )

        it "handles numbers" $
            tokenize "one (Finite 1)"
                `shouldBe` Right
                    ( [ PText "one"
                      , Parenthetical "Finite 1"
                      ]
                    )

        it "handles quotes" $
            tokenize "\"foo bar\" \"baz\""
                `shouldBe` Right
                    ( [ Quotation "foo bar"
                      , Quotation "baz"
                      ]
                    )

        it "handles SQL literals with no specified type" $
            tokenize "attr='[\"ab\\'cd\", 1, 2]'"
                `shouldBe` Right
                    ( [Equality "attr" "'[\"ab'cd\", 1, 2]'"]
                    )

        it "handles SQL literals with a specified type" $
            tokenize "attr='{\"\\'a\\'\": [1, 2.2, \"\\'3\\'\"]}'::type_name"
                `shouldBe` Right
                    ( [Equality "attr" "'{\"'a'\": [1, 2.2, \"'3'\"]}'::type_name"]
                    )

        it "should error if quotes are unterminated" $ do
            first errorBundlePretty (tokenize "\"foo bar")
                `shouldBe` Left
                    ( "1:9:\n  |\n1 | \"foo bar\n  |         ^\nunexpected end of input\nexpecting '\"' or literal character\n"
                    )

        it "handles commas in tokens" $
            tokenize "x=COALESCE(left,right)  \"baz\""
                `shouldBe` Right
                    ( [ Equality "x" "COALESCE(left,right)"
                      , Quotation "baz"
                      ]
                    )

        it "handles quotes mid-token" $
            tokenize "x=\"foo bar\"  \"baz\""
                `shouldBe` Right
                    ( [ Equality "x" "foo bar"
                      , Quotation "baz"
                      ]
                    )

        it "handles escaped quotes mid-token" $
            tokenize "x=\\\"foo bar\"  \"baz\""
                `shouldBe` Right
                    ( [ Equality "x" "\\\"foo"
                      , PText "bar\""
                      , Quotation "baz"
                      ]
                    )

        it "handles unnested parentheses" $
            tokenize "(foo bar)  (baz)"
                `shouldBe` Right
                    ( [ Parenthetical "foo bar"
                      , Parenthetical "baz"
                      ]
                    )

        it "handles unnested parentheses mid-token" $
            tokenize "x=(foo bar)  (baz)"
                `shouldBe` Right
                    ( [ Equality "x" "foo bar"
                      , Parenthetical "baz"
                      ]
                    )

        it "handles nested parentheses" $
            tokenize "(foo (bar))  (baz)"
                `shouldBe` Right
                    ( [ Parenthetical "foo (bar)"
                      , Parenthetical "baz"
                      ]
                    )

        it "handles escaped quotation marks in plain tokens" $
            tokenize "foo bar\\\"baz"
                `shouldBe` Right
                    ( [ PText "foo"
                      , PText "bar\\\"baz"
                      ]
                    )

        it "handles escaped quotation marks in quotations" $
            tokenize "foo \"bar\\\"baz\""
                `shouldBe` Right
                    ( [ PText "foo"
                      , Quotation "bar\"baz"
                      ]
                    )

        it "handles escaped quotation marks in equalities" $
            tokenize "y=\"baz\\\"\""
                `shouldBe` Right
                    ( [ Equality "y" "baz\""
                      ]
                    )

        it "handles escaped quotation marks in parentheticals" $
            tokenize "(foo \\\"bar)"
                `shouldBe` Right
                    ( [ Parenthetical "foo \\\"bar"
                      ]
                    )

        it "handles escaped parentheses in quotations" $
            tokenize "foo \"bar\\(baz\""
                `shouldBe` Right
                    ( [ PText "foo"
                      , Quotation "bar(baz"
                      ]
                    )

        it "handles escaped parentheses in plain tokens" $
            tokenize "foo bar\\(baz"
                `shouldBe` Right
                    ( [ PText "foo"
                      , PText "bar(baz"
                      ]
                    )

        it "handles escaped parentheses in parentheticals" $
            tokenize "(foo \\(bar)"
                `shouldBe` Right
                    ( [ Parenthetical "foo (bar"
                      ]
                    )

        it "handles escaped parentheses in equalities" $
            tokenize "y=baz\\("
                `shouldBe` Right
                    ( [ Equality "y" "baz("
                      ]
                    )

        it "handles mid-token quote in later token" $
            tokenize "foo bar baz=(bin\")"
                `shouldBe` Right
                    ( [ PText "foo"
                      , PText "bar"
                      , Equality "baz" "bin\""
                      ]
                    )

    describe "parse" $ do
        let
            subject =
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
        let
            [bicycle, car, vehicle] = defs subject

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
            let
                simplifyField field =
                    (unboundFieldNameHS field, unboundFieldNameDB field, unboundFieldComments field)
            (simplifyField <$> unboundEntityFields bicycle)
                `shouldBe` [ (FieldNameHS "brand", FieldNameDB "brand", Nothing)
                           ]
            (simplifyField <$> unboundEntityFields car)
                `shouldBe` [ (FieldNameHS "make", FieldNameDB "make", Just "the make of the Car\n")
                           , (FieldNameHS "model", FieldNameDB "model", Just "the model of the Car\n")
                           ]
            (simplifyField <$> unboundEntityFields vehicle)
                `shouldBe` [ (FieldNameHS "bicycle", FieldNameDB "bicycle", Nothing)
                           , (FieldNameHS "car", FieldNameDB "car", Nothing)
                           ]

        it "should parse the `entityUniques` field" $ do
            let
              simplifyUnique unique =
                (uniqueHaskell unique, uniqueFields unique)
            (simplifyUnique <$> entityUniques (unboundEntityDef bicycle)) `shouldBe` []
            (simplifyUnique <$> entityUniques (unboundEntityDef car))
              `shouldBe` [ (ConstraintNameHS "UniqueModel", [(FieldNameHS "model", FieldNameDB "model")])
                         ]
            (simplifyUnique <$> entityUniques (unboundEntityDef vehicle)) `shouldBe` []

        it "should parse the `entityForeigns` field" $ do
            let
                [user, notification] =
                    defs
                        [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text
    !yes            Int
    ~no             Int

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text

    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst emailSecond
|]
            unboundForeignDefs user `shouldBe` []
            map unboundForeignDef (unboundForeignDefs notification)
                `shouldBe` [ ForeignDef
                                { foreignRefTableHaskell = EntityNameHS "User"
                                , foreignRefTableDBName = EntityNameDB "user"
                                , foreignConstraintNameHaskell = ConstraintNameHS "fk_noti_user"
                                , foreignConstraintNameDBName = ConstraintNameDB "notificationfk_noti_user"
                                , foreignFieldCascade = FieldCascade Nothing Nothing
                                , foreignFields =
                                    []
                                , -- the foreign fields are not set yet in an unbound
                                  -- entity def
                                  foreignAttrs = []
                                , foreignNullable = False
                                , foreignToPrimary = False
                                }
                           ]

        it "should parse the `entityDerives` field" $ do
            entityDerives (unboundEntityDef bicycle) `shouldBe` ["Eq"]
            entityDerives (unboundEntityDef car) `shouldBe` ["Eq", "Show"]
            entityDerives (unboundEntityDef vehicle) `shouldBe` []

        it "should parse the `entityEntities` field" $ do
            entityExtra (unboundEntityDef bicycle)
                `shouldBe` Map.singleton "ExtraBike" [["foo", "bar"], ["baz"]]
            entityExtra (unboundEntityDef car) `shouldBe` mempty
            entityExtra (unboundEntityDef vehicle) `shouldBe` mempty

        it "should parse the `entitySum` field" $ do
            entitySum (unboundEntityDef bicycle) `shouldBe` False
            entitySum (unboundEntityDef car) `shouldBe` False
            entitySum (unboundEntityDef vehicle) `shouldBe` True

        it "should parse the `entityComments` field" $ do
            entityComments (unboundEntityDef bicycle) `shouldBe` Nothing
            entityComments (unboundEntityDef car) `shouldBe` Just "This is a Car\n"
            entityComments (unboundEntityDef vehicle) `shouldBe` Nothing

        it "should error on malformed input, unterminated parens" $ do
            let
                definitions =
                    [st|
User
    name Text
    age  (Maybe Int
|]
            let
                [user] = defs definitions
            evaluate (unboundEntityDef user)
                `shouldErrorWithMessage` "4:20:\n  |\n4 |     age  (Maybe Int\n  |                    ^\nunexpected newline\nexpecting '!', '\"', ''', '(', ')', ',', '-', '.', ':', '[', '\\', ']', '_', '~', alphanumeric character, space, or tab\n"

        it "errors on duplicate cascade update declarations" $ do
            let
                definitions =
                    [st|
User
    age  Int OnUpdateCascade OnUpdateCascade
|]
            let
                [user] = defs definitions
            mapM (evaluate . unboundFieldCascade) (unboundEntityFields user)
                `shouldErrorWithMessage` "found more than one OnUpdate action, tokens: [\"OnUpdateCascade\",\"OnUpdateCascade\"]"

        it "errors on duplicate cascade delete declarations" $ do
            let
                definitions =
                    [st|
User
    age  Int OnDeleteCascade OnDeleteCascade
|]
            let
                [user] = defs definitions
            mapM (evaluate . unboundFieldCascade) (unboundEntityFields user)
                `shouldErrorWithMessage` "found more than one OnDelete action, tokens: [\"OnDeleteCascade\",\"OnDeleteCascade\"]"

        describe "custom Id column" $ do
            it "parses custom Id column" $ do
                let
                    definitions =
                        [st|
User
    Id   Text
    name Text
    age  Int
|]
                let
                    [user] = defs definitions
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"
                entityDB (unboundEntityDef user) `shouldBe` EntityNameDB "user"
                let
                    idFields = NEL.toList (entitiesPrimary (unboundEntityDef user))
                (fieldHaskell <$> idFields) `shouldBe` [FieldNameHS "Id"]
                (fieldDB <$> idFields) `shouldBe` [FieldNameDB "id"]
                (fieldType <$> idFields) `shouldBe` [FTTypeCon Nothing "Text"]
                (unboundFieldNameHS <$> unboundEntityFields user)
                    `shouldBe` [ FieldNameHS "name"
                               , FieldNameHS "age"
                               ]

            it "errors on duplicate custom Id column" $ do
                let
                    definitions =
                        [st|
User
    Id   Text
    Id   Text
    name Text
    age  Int
|]
                let
                    [user] = defs definitions
                    errMsg = [st|expected only one Id declaration per entity|]
                evaluate (unboundEntityDef user)
                    `shouldErrorWithMessage` (T.unpack errMsg)

        describe "primary declaration" $ do
            it "parses Primary declaration" $ do
                let
                    definitions =
                        [st|
User
    ref Text
    name Text
    age  Int
    Primary ref
|]
                let
                    [user] = defs definitions
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"
                entityDB (unboundEntityDef user) `shouldBe` EntityNameDB "user"
                let
                    idFields = NEL.toList (entitiesPrimary (unboundEntityDef user))
                (fieldHaskell <$> idFields) `shouldBe` [FieldNameHS "Id"]
                (fieldDB <$> idFields) `shouldBe` [FieldNameDB "id"]
                (fieldType <$> idFields) `shouldBe` [FTTypeCon Nothing "UserId"]
                (unboundFieldNameHS <$> unboundEntityFields user)
                    `shouldBe` [ FieldNameHS "ref"
                               , FieldNameHS "name"
                               , FieldNameHS "age"
                               ]
                entityUniques (unboundEntityDef user)
                    `shouldBe` [ UniqueDef
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
                let
                    definitions =
                        [st|
User
    ref Text
    name Text
    age  Int
    Primary ref
    Primary name
|]
                let
                    [user] = defs definitions
                    errMsg = "expected only one Primary declaration per entity"
                evaluate (unboundEntityDef user)
                    `shouldErrorWithMessage` errMsg

            it "errors on conflicting Primary/Id declarations" $ do
                let
                    definitions =
                        [st|
User
    Id Text
    ref Text
    name Text
    age  Int
    Primary ref
|]
                let
                    [user] = defs definitions
                    errMsg = [st|Specified both an ID field and a Primary field|]
                evaluate (unboundEntityDef user)
                    `shouldErrorWithMessage` (T.unpack errMsg)

            it "triggers error on invalid declaration" $ do
                let
                    definitions =
                        [st|
User
    age Text
    Primary ref
|]
                let
                    [user] = defs definitions
                case unboundPrimarySpec user of
                    NaturalKey ucd -> do
                        evaluate (NEL.head $ unboundCompositeCols ucd)
                            `shouldErrorWithMessage` "Unknown column in primary key constraint: \"ref\""
                    _ ->
                        error "Expected NaturalKey, failing"

        describe "entity unique constraints" $ do
            it "triggers error if declared field does not exist" $ do
                let
                    definitions =
                        [st|
User
    name            Text
    emailFirst      Text

    UniqueEmail emailFirst emailSecond
|]
                let
                    [user] = defs definitions
                    uniques = entityUniques (unboundEntityDef user)
                    [dbNames] = fmap snd . uniqueFields <$> uniques
                    errMsg =
                        unwords
                            [ "Unknown column in \"UniqueEmail\" constraint: \"emailSecond\""
                            , "possible fields: [\"name\",\"emailFirst\"]"
                            ]
                evaluate (head (NEL.tail dbNames))
                    `shouldErrorWithMessage` errMsg

            it "triggers error if no valid constraint name provided" $ do
                let
                    definitions =
                        [st|
User
    age Text
    Unique some
|]
                let
                    [user] = defs definitions
                evaluate (unboundPrimarySpec user)
                    `shouldErrorWithMessage` "invalid unique constraint on table[\"User\"] expecting an uppercase constraint name xs=[\"some\"]"

        describe "foreign keys" $ do
            let
                validDefinitions =
                    [st|
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
                    [_user, notification] = defsWithSettings (setPsToFKName flippedFK lowerCaseSettings) validDefinitions
                    [notificationForeignDef] =
                        unboundForeignDef <$> unboundForeignDefs notification
                foreignConstraintNameDBName notificationForeignDef
                    `shouldBe` ConstraintNameDB "fk_noti_user_notification"

            it "should error when insufficient params provided" $ do
                let
                    definitions =
                        [st|
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
                let
                    [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage` "invalid foreign key constraint on table[\"Notification\"] expecting a lower case constraint name or a cascading action xs=[]"

            it "should error when foreign fields not provided" $ do
                let
                    definitions =
                        [st|
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
                let
                    [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage` "No fields on foreign reference."

            it "should error when number of parent and foreign fields differ" $ do
                let
                    definitions =
                        [st|
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
                let
                    [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage` "invalid foreign key constraint on table[\"Notification\"] Found 2 foreign fields but 1 parent fields"

            it
                "should throw error when there is more than one delete cascade on the declaration"
                $ do
                    let
                        definitions =
                            [st|
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
                    let
                        [_user, notification] = defsSnake definitions
                    mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                        `shouldErrorWithMessage` "invalid foreign key constraint on table[\"Notification\"] found more than one OnDelete actions"

            it
                "should throw error when there is more than one update cascade on the declaration"
                $ do
                    let
                        definitions =
                            [st|
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
                    let
                        [_user, notification] = defsSnake definitions
                    mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                        `shouldErrorWithMessage` "invalid foreign key constraint on table[\"Notification\"] found more than one OnUpdate actions"

            it
                "should allow you to enable snake cased foriegn keys via a preset configuration function"
                $ do
                    let
                        [_user, notification] =
                            defsSnake validDefinitions
                        [notificationForeignDef] =
                            unboundForeignDef <$> unboundForeignDefs notification
                    foreignConstraintNameDBName notificationForeignDef
                        `shouldBe` ConstraintNameDB "notification_fk_noti_user"

        describe "ticked types" $ do
            it "should be able to parse ticked types" $ do
                let
                    simplifyField field =
                        (unboundFieldNameHS field, unboundFieldType field)
                let
                    tickedDefinition =
                        [st|
CustomerTransfer
    customerId CustomerId
    moneyAmount (MoneyAmount 'Customer 'Debit)
    currencyCode CurrencyCode
    uuid TransferUuid
|]
                let
                    [customerTransfer] = defs tickedDefinition
                let
                    expectedType =
                        FTTypeCon Nothing "MoneyAmount"
                            `FTApp` FTTypePromoted "Customer"
                            `FTApp` FTTypePromoted "Debit"

                (simplifyField <$> unboundEntityFields customerTransfer)
                    `shouldBe` [ (FieldNameHS "customerId", FTTypeCon Nothing "CustomerId")
                               , (FieldNameHS "moneyAmount", expectedType)
                               , (FieldNameHS "currencyCode", FTTypeCon Nothing "CurrencyCode")
                               , (FieldNameHS "uuid", FTTypeCon Nothing "TransferUuid")
                               ]

        describe "type literals" $ do
            it "should be able to parse type literals" $ do
                let
                    simplifyField field =
                        (unboundFieldNameHS field, unboundFieldType field)
                let
                    tickedDefinition =
                        [st|
WithFinite
    one    (Finite 1)
    twenty (Labelled "twenty")
|]
                let
                    [withFinite] = defs tickedDefinition

                (simplifyField <$> unboundEntityFields withFinite)
                    `shouldBe` [ (FieldNameHS "one", FTApp (FTTypeCon Nothing "Finite") (FTLit (IntTypeLit 1)))
                               ,
                                   ( FieldNameHS "twenty"
                                   , FTApp (FTTypeCon Nothing "Labelled") (FTLit (TextTypeLit "twenty"))
                                   )
                               ]

    describe "parseFieldType" $ do
        it "simple types" $
            parseFieldType "FooBar" `shouldBe` Right (FTTypeCon Nothing "FooBar")
        it "module types" $
            parseFieldType "Data.Map.FooBar"
                `shouldBe` Right (FTTypeCon (Just "Data.Map") "FooBar")
        it "application" $
            parseFieldType "Foo Bar"
                `shouldBe` Right
                    ( FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar"
                    )
        it "application multiple" $
            parseFieldType "Foo Bar Baz"
                `shouldBe` Right
                    ( (FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
                        `FTApp` FTTypeCon Nothing "Baz"
                    )
        it "parens" $ do
            let
                foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo (Bar Baz)"
                `shouldBe` Right
                    ( foo `FTApp` (bar `FTApp` baz)
                    )
        it "lists" $ do
            let
                foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                bars = FTList bar
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo [Bar] Baz"
                `shouldBe` Right
                    ( foo `FTApp` bars `FTApp` baz
                    )
        it "numeric type literals" $ do
            let
                expected = FTApp (FTTypeCon Nothing "Finite") (FTLit (IntTypeLit 1))
            parseFieldType "Finite 1" `shouldBe` Right expected
        it "string type literals" $ do
            let
                expected = FTApp (FTTypeCon Nothing "Labelled") (FTLit (TextTypeLit "twenty"))
            parseFieldType "Labelled \"twenty\"" `shouldBe` Right expected
        it "nested list / parens (list inside parens)" $ do
            let
                maybeCon = FTTypeCon Nothing "Maybe"
                int = FTTypeCon Nothing "Int"
            parseFieldType "Maybe (Maybe [Int])"
                `shouldBe` Right
                    (maybeCon `FTApp` (maybeCon `FTApp` FTList int))
        it "nested list / parens (parens inside list)" $ do
            let
                maybeCon = FTTypeCon Nothing "Maybe"
                int = FTTypeCon Nothing "Int"
            parseFieldType "[Maybe (Maybe Int)]"
                `shouldBe` Right
                    (FTList (maybeCon `FTApp` (maybeCon `FTApp` int)))
        it "fails on lowercase starts" $ do
            parseFieldType "nothanks" `shouldBe` Left "PSFail \"nothanks\""

    describe "#1175 empty entity" $ do
        let
            subject =
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

        it "parse works" $ do
            let
                test name'fieldCount parsedList = do
                    case (name'fieldCount, parsedList) of
                        ([], []) ->
                            pure ()
                        ((name, fieldCount) : _, []) ->
                            expectationFailure $
                                "Expected an entity with name "
                                    <> name
                                    <> " and "
                                    <> show fieldCount
                                    <> " fields"
                                    <> ", but the list was empty..."
                        ((name, fieldCount) : ys, (x : xs)) -> do
                            let
                                UnboundEntityDef{..} =
                                    x
                            (unEntityNameHS (getUnboundEntityNameHS x), length unboundEntityFields)
                                `shouldBe` (T.pack name, fieldCount)
                            test ys xs
                        ([], _ : _) ->
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
