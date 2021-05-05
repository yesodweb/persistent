{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Persist.QuasiSpec where

import Prelude hiding (lines)

import Data.List hiding (lines)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import qualified Data.Text as T
import Database.Persist.EntityDef.Internal
import Database.Persist.Quasi
import Database.Persist.Quasi.Internal
import Database.Persist.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Shakespeare.Text (st)

spec :: Spec
spec = describe "Quasi" $ do
    describe "splitExtras" $ do
        let helloWorldTokens = Token "hello" :| [Token "world"]
            foobarbazTokens = Token "foo" :| [Token "bar", Token "baz"]
        it "works" $ do
            splitExtras []
                `shouldBe`
                    mempty
        it "works2" $ do
            splitExtras
                [ Line 0 helloWorldTokens
                ]
                `shouldBe`
                    ( [NEL.toList helloWorldTokens], mempty )
        it "works3" $ do
            splitExtras
                [ Line 0 helloWorldTokens
                , Line 2 foobarbazTokens
                ]
                `shouldBe`
                    ( [NEL.toList helloWorldTokens, NEL.toList foobarbazTokens], mempty )
        it "works4" $ do
            splitExtras
                [ Line 0 [Token "Product"]
                , Line 2 (Token <$> ["name", "Text"])
                , Line 2 (Token <$> ["added", "UTCTime", "default=CURRENT_TIMESTAMP"])
                ]
                `shouldBe`
                    ( []
                    , Map.fromList
                        [ ("Product",
                            [ ["name", "Text"]
                            , ["added", "UTCTime", "default=CURRENT_TIMESTAMP"]
                            ]
                        ) ]
                    )
        it "works5" $ do
            splitExtras
                [ Line 0 [Token "Product"]
                , Line 2 (Token <$> ["name", "Text"])
                , Line 4 [Token "ExtraBlock"]
                , Line 4 (Token <$> ["added", "UTCTime", "default=CURRENT_TIMESTAMP"])
                ]
                `shouldBe`
                    ( []
                    , Map.fromList
                        [ ("Product",
                            [ ["name", "Text"]
                            , ["ExtraBlock"]
                            , ["added", "UTCTime", "default=CURRENT_TIMESTAMP"]
                            ]
                        )]
                    )
    describe "takeColsEx" $ do
        let subject = takeColsEx upperCaseSettings
        it "fails on a single word" $ do
            subject ["asdf"]
                `shouldBe`
                    Nothing
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
                    ( Line 1
                        [ Token "foo"
                        , Token "bar"
                        , Token "baz"
                        ]
                    )

        it "handles quotes" $
            parseLine "  \"foo bar\"  \"baz\"" `shouldBe`
                Just
                    ( Line 2
                        [ Token "foo bar"
                        , Token "baz"
                        ]
                    )

        it "handles quotes mid-token" $
            parseLine "  x=\"foo bar\"  \"baz\"" `shouldBe`
                Just
                    ( Line 2
                        [ Token "x=foo bar"
                        , Token "baz"
                        ]
                    )

        it "handles escaped quote mid-token" $
            parseLine "  x=\\\"foo bar\"  \"baz\"" `shouldBe`
                Just
                    ( Line 2
                        [ Token "x=\\\"foo"
                        , Token "bar\""
                        , Token "baz"
                        ]
                    )

        it "handles unnested parantheses" $
            parseLine "  (foo bar)  (baz)" `shouldBe`
                Just
                    ( Line 2
                        [ Token "foo bar"
                        , Token "baz"
                        ]
                    )

        it "handles unnested parantheses mid-token" $
            parseLine "  x=(foo bar)  (baz)" `shouldBe`
                Just
                    ( Line 2
                        [ Token "x=foo bar"
                        , Token "baz"
                        ]
                    )

        it "handles nested parantheses" $
            parseLine "  (foo (bar))  (baz)" `shouldBe`
                Just
                    ( Line 2
                        [ Token "foo (bar)"
                        , Token "baz"
                        ]
                    )

        it "escaping" $
            parseLine "  (foo \\(bar)  y=\"baz\\\"\"" `shouldBe`
                Just
                    ( Line 2
                        [ Token "foo (bar"
                        , Token "y=baz\""
                        ]
                    )

        it "mid-token quote in later token" $
            parseLine "foo bar baz=(bin\")" `shouldBe`
                Just
                    ( Line 0
                        [ Token "foo"
                        , Token "bar"
                        , Token "baz=bin\""
                        ]
                    )

        describe "comments" $ do
            it "recognizes one line" $ do
                parseLine "-- | this is a comment" `shouldBe`
                    Just
                        ( Line 0
                            [ DocComment "this is a comment"
                            ]
                        )

            it "works if comment is indented" $ do
                parseLine "  -- | comment" `shouldBe`
                    Just (Line 2 [DocComment "comment"])

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
        let [bicycle, car, vehicle] = parse lowerCaseSettings subject

        it "should parse the `entityHaskell` field" $ do
            getUnboundEntityNameHS bicycle `shouldBe` EntityNameHS "Bicycle"
            getUnboundEntityNameHS car `shouldBe` EntityNameHS "Car"
            getUnboundEntityNameHS vehicle `shouldBe` EntityNameHS "Vehicle"

        it "should parse the `entityDB` field" $ do
            entityDB (unboundEntityDef bicycle) `shouldBe` EntityNameDB "bicycle"
            entityDB (unboundEntityDef car) `shouldBe` EntityNameDB "car"
            entityDB (unboundEntityDef vehicle) `shouldBe` EntityNameDB "vehicle"

        it "should parse the `entityAttrs` field" $ do
            entityAttrs (unboundEntityDef bicycle) `shouldBe` ["-- | this is a bike"]
            entityAttrs (unboundEntityDef car) `shouldBe` []
            entityAttrs (unboundEntityDef vehicle) `shouldBe` []

        it "should parse the `unboundEntityFields` field" $ do
            let simplifyField field =
                    (unboundFieldNameHS field, unboundFieldNameDB field, unboundFieldComments field)
            (simplifyField <$> unboundEntityFields bicycle) `shouldBe`
                [ (FieldNameHS "brand", FieldNameDB "brand", Nothing)
                ]
            (simplifyField <$> unboundEntityFields car) `shouldBe`
                [ (FieldNameHS "make", FieldNameDB "make", Just "the make of the Car\n")
                , (FieldNameHS "model", FieldNameDB "model", Just "the model of the Car\n")
                ]
            (simplifyField <$> unboundEntityFields vehicle) `shouldBe`
                [ (FieldNameHS "bicycle", FieldNameDB "bicycle", Nothing)
                , (FieldNameHS "car", FieldNameDB "car", Nothing)
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
            let [user, notification] = parse lowerCaseSettings [st|
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
            entityExtra (unboundEntityDef bicycle) `shouldBe` Map.singleton "ExtraBike" [["foo", "bar", "-- | this is a foo bar"], ["baz"]]
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

        describe "foreign keys" $ do
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

    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst emailSecond
|]

            it "should allow you to modify the FK name via provided function" $ do
                let
                    flippedFK (EntityNameHS entName) (ConstraintNameHS conName) =
                        conName <> entName
                    [_user, notification] =
                        parse (setPsToFKName flippedFK lowerCaseSettings) definitions
                    [notificationForeignDef] =
                        unboundForeignDef <$> unboundForeignDefs notification
                foreignConstraintNameDBName notificationForeignDef
                    `shouldBe`
                        ConstraintNameDB "fk_noti_user_notification"

            it "should allow you to enable snake cased foriegn keys via a preset configuration function" $ do
                let
                    [_user, notification] =
                        parse (setPsUseSnakeCaseForiegnKeys lowerCaseSettings) definitions
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
                let [customerTransfer] = parse lowerCaseSettings tickedDefinition
                let expectedType =
                        FTTypeCon Nothing "MoneyAmount" `FTApp` FTTypePromoted "Customer" `FTApp` FTTypePromoted "Debit"

                (simplifyField <$> unboundEntityFields customerTransfer) `shouldBe`
                    [ (FieldNameHS "customerId", FTTypeCon Nothing "CustomerId")
                    , (FieldNameHS "moneyAmount", expectedType)
                    , (FieldNameHS "currencyCode", FTTypeCon Nothing "CurrencyCode")
                    , (FieldNameHS "uuid", FTTypeCon Nothing "TransferUuid")
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
        it "fails on lowercase starts" $ do
            parseFieldType "nothanks" `shouldBe` Left "PSFail ('n',\"othanks\")"

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

        let preparsed =
                preparse subject
        it "preparse works" $ do
            (length <$> preparsed) `shouldBe` Just 10

        let fooLines =
                [ Line
                    { lineIndent = 0
                    , tokens = Token "Foo" :| []
                    }
                , Line
                    { lineIndent = 4
                    , tokens = Token "name" :| [Token "String"]
                    }
                , Line
                    { lineIndent = 4
                    , tokens = Token "age" :| [Token "Int"]
                    }
                ]
            emptyLines =
                [ Line
                    { lineIndent = 0
                    , tokens = Token "EmptyEntity" :| []
                    }
                ]
            barLines =
                [ Line
                    { lineIndent = 0
                    , tokens = Token "Bar" :| []
                    }
                , Line
                    { lineIndent = 4
                    , tokens = Token "name" :| [Token "String"]
                    }
                ]
            bazLines =
                [ Line
                    { lineIndent = 0
                    , tokens = Token "Baz" :| []
                    }
                , Line
                    { lineIndent = 4
                    , tokens = Token "a" :| [Token "Int"]
                    }
                , Line
                    { lineIndent = 4
                    , tokens = Token "b" :| [Token "String"]
                    }
                , Line
                    { lineIndent = 4
                    , tokens = Token "c" :| [Token "FooId"]
                    }
                ]

        let
            linesAssociated =
                case preparsed of
                    Nothing -> error "preparsed failed"
                    Just lines -> associateLines lines
        it "associateLines works" $ do
            linesAssociated `shouldMatchList`
                [ LinesWithComments
                    { lwcLines = NEL.fromList fooLines
                    , lwcComments = []
                    }
                , LinesWithComments (NEL.fromList emptyLines) []
                , LinesWithComments (NEL.fromList barLines) []
                , LinesWithComments (NEL.fromList bazLines) []
                ]

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
                    parse lowerCaseSettings subject
            length result `shouldBe` 4

            test
                [ ("Foo", 2)
                , ("EmptyEntity", 0)
                , ("Bar", 1)
                , ("Baz", 3)
                ]
                result


    describe "preparse" $ do
        prop "omits lines that are only whitespace" $ \len -> do
            ws <- vectorOf len arbitraryWhiteSpaceChar
            pure $ preparse (T.pack ws) === Nothing

        it "recognizes entity" $ do
            let expected =
                    Line { lineIndent = 0, tokens = pure (Token "Person") } :|
                    [ Line { lineIndent = 2, tokens = Token "name" :| [Token "String"] }
                    , Line { lineIndent = 2, tokens = Token "age" :| [Token "Int"] }
                    ]
            preparse "Person\n  name String\n  age Int" `shouldBe` Just expected

        it "recognizes comments" $ do
            let text = "Foo\n  x X\n-- | Hello\nBar\n name String"
            let expected =
                    Line { lineIndent = 0, tokens = pure (Token "Foo") } :|
                    [ Line { lineIndent = 2, tokens = Token "x" :| [Token "X"] }
                    , Line { lineIndent = 0, tokens = pure (DocComment "Hello") }
                    , Line { lineIndent = 0, tokens = pure (Token "Bar") }
                    , Line { lineIndent = 1, tokens = Token "name" :| [Token "String"] }
                    ]
            preparse text `shouldBe` Just expected

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
                    Line { lineIndent = 2, tokens = pure (Token "Foo") } :|
                    [ Line { lineIndent = 4, tokens = Token "x" :| [Token "X"] }
                    , Line { lineIndent = 2, tokens = pure (DocComment "Comment") }
                    , Line { lineIndent = 2, tokens = pure (Token "Bar") }
                    , Line { lineIndent = 4, tokens = Token "name" :| [Token "String"] }
                    ]
            preparse t `shouldBe` Just expected

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
                    Line { lineIndent = 0, tokens = pure (Token "LowerCaseTable") } :|
                    [ Line { lineIndent = 2, tokens = Token "name" :| [Token "String"] }
                    , Line { lineIndent = 2, tokens = pure (Token "ExtraBlock") }
                    , Line { lineIndent = 4, tokens = Token "foo" :| [Token "bar"] }
                    , Line { lineIndent = 4, tokens = pure (Token "baz") }
                    , Line { lineIndent = 2, tokens = pure (Token "ExtraBlock2") }
                    , Line { lineIndent = 4, tokens = pure (Token "something") }
                    ]
            preparse t `shouldBe` Just expected

        it "field comments" $ do
            let text = T.unlines
                    [ "-- | Model"
                    , "Foo"
                    , "  -- | Field"
                    , "  name String"
                    ]
                expected =
                    Line { lineIndent = 0, tokens = [DocComment "Model"] } :|
                    [ Line { lineIndent = 0, tokens = [Token "Foo"] }
                    , Line { lineIndent = 2, tokens = [DocComment "Field"] }
                    , Line { lineIndent = 2, tokens = (Token <$> ["name", "String"]) }
                    ]
            preparse text `shouldBe` Just expected

    describe "associateLines" $ do
        let foo =
                Line
                    { lineIndent = 0
                    , tokens = pure (Token "Foo")
                    }
            name'String =
                Line
                    { lineIndent = 2
                    , tokens = Token "name" :| [Token "String"]
                    }
            comment =
                Line
                    { lineIndent = 0
                    , tokens = pure (DocComment "comment")
                    }
        it "works" $ do
            associateLines
                ( comment :|
                [ foo
                , name'String
                ])
                `shouldBe`
                    [ LinesWithComments
                        { lwcComments = ["comment"]
                        , lwcLines = foo :| [name'String]
                        }
                    ]
        let bar =
                Line
                    { lineIndent = 0
                    , tokens = Token "Bar" :| [Token "sql", Token "=", Token "bars"]
                    }
            age'Int =
                Line
                    { lineIndent = 1
                    , tokens = Token "age" :| [Token "Int"]
                    }
        it "works when used consecutively" $ do
            associateLines
                ( bar :|
                [ age'Int
                , comment
                , foo
                , name'String
                ])
                `shouldBe`
                    [ LinesWithComments
                        { lwcComments = []
                        , lwcLines = bar :| [age'Int]
                        }
                    , LinesWithComments
                        { lwcComments = ["comment"]
                        , lwcLines = foo :| [name'String]
                        }
                    ]
        it "works with textual input" $ do
            let text = preparse "Foo\n  x X\n-- | Hello\nBar\n name String"
            associateLines <$> text
                `shouldBe` Just
                    [ LinesWithComments
                        { lwcLines =
                            Line {lineIndent = 0, tokens = Token "Foo" :| []}
                            :| [ Line {lineIndent = 2, tokens = Token "x" :| [Token "X"]} ]
                        , lwcComments =
                            []
                        }
                    , LinesWithComments
                        { lwcLines =
                            Line {lineIndent = 0, tokens = Token "Bar" :| []}
                            :| [ Line {lineIndent = 1, tokens = Token "name" :| [Token "String"]}]
                        , lwcComments =
                            ["Hello"]
                        }
                    ]
        it "works with extra blocks" $ do
            let text = preparse . T.unlines $
                    [ "LowerCaseTable"
                    , "    Id             sql=my_id"
                    , "    fullName Text"
                    , "    ExtraBlock"
                    , "        foo bar"
                    , "        baz"
                    , "        bin"
                    , "    ExtraBlock2"
                    , "        something"
                    ]
            associateLines <$> text `shouldBe` Just
                [ LinesWithComments
                    { lwcLines =
                        Line { lineIndent = 0, tokens = pure (Token "LowerCaseTable") } :|
                        [ Line { lineIndent = 4, tokens = Token "Id" :| [Token "sql=my_id"] }
                        , Line { lineIndent = 4, tokens = Token "fullName" :| [Token "Text"] }
                        , Line { lineIndent = 4, tokens = pure (Token "ExtraBlock") }
                        , Line { lineIndent = 8, tokens = Token "foo" :| [Token "bar"] }
                        , Line { lineIndent = 8, tokens = pure (Token "baz") }
                        , Line { lineIndent = 8, tokens = pure (Token "bin") }
                        , Line { lineIndent = 4, tokens = pure (Token "ExtraBlock2") }
                        , Line { lineIndent = 8, tokens = pure (Token "something") }
                        ]
                    , lwcComments = []
                    }
                ]

        it "works with extra blocks twice" $ do
            let text = preparse . T.unlines $
                    [ "IdTable"
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
                    ]
            associateLines <$> text `shouldBe` Just
                [ LinesWithComments
                    { lwcLines = Line 0 (pure (Token "IdTable")) :|
                        [ Line 4 (Token "Id" <| Token "Day" :| [Token "default=CURRENT_DATE"])
                        , Line 4 (Token "name" :| [Token "Text"])
                        ]
                    , lwcComments = []
                    }
                , LinesWithComments
                    { lwcLines =
                        Line { lineIndent = 0, tokens = pure (Token "LowerCaseTable") } :|
                        [ Line { lineIndent = 4, tokens = Token "Id" :| [Token "sql=my_id"] }
                        , Line { lineIndent = 4, tokens = Token "fullName" :| [Token "Text"] }
                        , Line { lineIndent = 4, tokens = pure (Token "ExtraBlock") }
                        , Line { lineIndent = 8, tokens = Token "foo" :| [Token "bar"] }
                        , Line { lineIndent = 8, tokens = pure (Token "baz") }
                        , Line { lineIndent = 8, tokens = pure (Token "bin") }
                        , Line { lineIndent = 4, tokens = pure (Token "ExtraBlock2") }
                        , Line { lineIndent = 8, tokens = pure (Token "something") }
                        ]
                    , lwcComments = []
                    }
                ]


        it "works with field comments" $ do
            let text = preparse . T.unlines $
                    [ "-- | Model"
                    , "Foo"
                    , "  -- | Field"
                    , "  name String"
                    ]
            associateLines <$> text `shouldBe` Just
                [ LinesWithComments
                    { lwcLines =
                        Line { lineIndent = 0, tokens = (Token "Foo") :| [] } :|
                            [ Line { lineIndent = 2, tokens = pure (DocComment "Field") }
                            , Line { lineIndent = 2, tokens = Token "name" :| [Token "String"] }
                            ]
                    , lwcComments =
                        ["Model"]
                    }
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
        let [subject] = parse lowerCaseSettings lines
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
                    case parse lowerCaseSettings $ T.unlines
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
