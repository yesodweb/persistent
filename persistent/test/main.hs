{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Char as Char
import qualified Data.Text as T
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
#if !MIN_VERSION_base(4,11,0)
-- This can be removed when GHC < 8.2.2 isn't supported anymore
import Data.Semigroup ((<>))
#endif
import Data.Time
import Text.Shakespeare.Text

import Database.Persist.Class.PersistField
import Database.Persist.Quasi
import Database.Persist.Types

main :: IO ()
main = hspec $ do
    describe "splitExtras" $ do
        let helloWorldTokens = asTokens ["hello", "world"]
            foobarbazTokens = asTokens ["foo", "bar", "baz"]
        it "works" $ do
            splitExtras []
                `shouldBe`
                    mempty
        it "works2" $ do
            splitExtras
                [ Line 0 helloWorldTokens
                ]
                `shouldBe`
                    ( [helloWorldTokens], mempty )
        it "works3" $ do
            splitExtras
                [ Line 0 helloWorldTokens
                , Line 2 foobarbazTokens
                ]
                `shouldBe`
                    ( [helloWorldTokens, foobarbazTokens], mempty )
        it "works4" $ do
            let foobarbarz = ["foo", "Bar", "baz"]
            splitExtras
                [ Line 0 [Token "Hello"]
                , Line 2 (asTokens foobarbarz)
                , Line 2 (asTokens foobarbarz)
                ]
                `shouldBe`
                    ( []
                    , Map.fromList
                        [ ("Hello", [foobarbarz, foobarbarz])
                        ]
                    )
        it "works5" $ do
            let foobarbarz = ["foo", "Bar", "baz"]
            splitExtras
                [ Line 0 (asTokens ["Hello"])
                , Line 2 (asTokens foobarbarz)
                , Line 4 (asTokens foobarbarz)
                ]
                `shouldBe`
                    ( []
                    , Map.fromList
                        [ ("Hello", [foobarbarz, foobarbarz])
                        ]
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
                    Just FieldDef
                        { fieldHaskell = FieldNameHS "asdf"
                        , fieldDB = FieldNameDB "asdf"
                        , fieldType = FTTypeCon Nothing "Int"
                        , fieldSqlType = SqlOther "SqlType unset for asdf"
                        , fieldAttrs = []
                        , fieldStrict = True
                        , fieldReference = NoReference
                        , fieldCascade = noCascade
                        , fieldComments = Nothing
                        , fieldGenerated = Nothing
                        }
        it "works if it has a name, type, and cascade" $ do
            subject ["asdf", "Int", "OnDeleteCascade", "OnUpdateCascade"]
                `shouldBe`
                    Just FieldDef
                        { fieldHaskell = FieldNameHS "asdf"
                        , fieldDB = FieldNameDB "asdf"
                        , fieldType = FTTypeCon Nothing "Int"
                        , fieldSqlType = SqlOther "SqlType unset for asdf"
                        , fieldAttrs = []
                        , fieldStrict = True
                        , fieldReference = NoReference
                        , fieldCascade = FieldCascade (Just Cascade) (Just Cascade)
                        , fieldComments = Nothing
                        , fieldGenerated = Nothing
                        }
        it "never tries to make a refernece" $ do
            subject ["asdf", "UserId", "OnDeleteCascade"]
                `shouldBe`
                    Just FieldDef
                        { fieldHaskell = FieldNameHS "asdf"
                        , fieldDB = FieldNameDB "asdf"
                        , fieldType = FTTypeCon Nothing "UserId"
                        , fieldSqlType = SqlOther "SqlType unset for asdf"
                        , fieldAttrs = []
                        , fieldStrict = True
                        , fieldReference = NoReference
                        , fieldCascade = FieldCascade Nothing (Just Cascade)
                        , fieldComments = Nothing
                        , fieldGenerated = Nothing
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

            it "map parseLine" $ do
                mapM parseLine ["Foo", "-- | Hello"]
                    `shouldBe`
                        Just
                            [ Line 0 [Token "Foo"]
                            , Line 0 [DocComment "Hello"]
                            ]

            it "works if comment is indented" $ do
                parseLine "  -- | comment" `shouldBe`
                    Just (Line 2 [ DocComment "comment"])

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

        let skippedEmpty =
                maybe [] skipEmpty preparsed
            fooLines =
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
            resultLines =
                concat
                    [ fooLines
                    , emptyLines
                    , barLines
                    , bazLines
                    ]

        it "skipEmpty works" $ do
            skippedEmpty `shouldBe` resultLines

        let linesAssociated =
                associateLines skippedEmpty
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

        let parsed =
                parse lowerCaseSettings subject
        it "parse works" $ do
            let test name'fieldCount xs = do
                    case (name'fieldCount, xs) of
                        ([], []) ->
                            pure ()
                        ((name, fieldCount) : _, []) ->
                            expectationFailure
                                $ "Expected an entity with name "
                                <> name
                                <> " and " <> show fieldCount <> " fields"
                                <> ", but the list was empty..."

                        ((name, fieldCount) : ys, (EntityDef {..} : xs)) -> do
                            (unEntityNameHS entityHaskell, length entityFields)
                                `shouldBe`
                                    (T.pack name, fieldCount)
                            test ys xs

                result =
                    parse lowerCaseSettings subject
            length parsed `shouldBe` 4

            test
                [ ("Foo", 2)
                , ("EmptyEntity", 0)
                , ("Bar", 1)
                , ("Baz", 3)
                ]
                parsed


    describe "preparse" $ do
        prop "omits lines that are only text" $ \len -> do
            ws <- vectorOf len arbitraryWhiteSpaceChar
            pure $ preparse (T.pack ws) === Nothing

        it "recognizes entity" $ do
            let expected =
                    Line { lineIndent = 0, tokens = asTokens ["Person"] } :|
                    [ Line { lineIndent = 2, tokens = asTokens ["name", "String"] }
                    , Line { lineIndent = 2, tokens = asTokens ["age", "Int"] }
                    ]
            preparse "Person\n  name String\n  age Int" `shouldBe` Just expected

        it "recognizes comments" $ do
            let text = "Foo\n  x X\n-- | Hello\nBar\n name String"
            let expected =
                    Line { lineIndent = 0, tokens = asTokens ["Foo"] } :|
                    [ Line { lineIndent = 2, tokens = asTokens ["x", "X"] }
                    , Line { lineIndent = 0, tokens = [DocComment "Hello"] }
                    , Line { lineIndent = 0, tokens = asTokens ["Bar"] }
                    , Line { lineIndent = 1, tokens = asTokens ["name", "String"] }
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
                    Line { lineIndent = 2, tokens = asTokens ["Foo"] } :|
                    [ Line { lineIndent = 4, tokens = asTokens ["x", "X"] }
                    , Line { lineIndent = 2, tokens = [DocComment "Comment"] }
                    , Line { lineIndent = 2, tokens = asTokens ["Bar"] }
                    , Line { lineIndent = 4, tokens = asTokens ["name", "String"] }
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
                    Line { lineIndent = 0, tokens = asTokens ["LowerCaseTable"] } :|
                    [ Line { lineIndent = 2, tokens = asTokens ["name", "String"] }
                    , Line { lineIndent = 2, tokens = asTokens ["ExtraBlock"] }
                    , Line { lineIndent = 4, tokens = asTokens ["foo", "bar"] }
                    , Line { lineIndent = 4, tokens = asTokens ["baz"] }
                    , Line { lineIndent = 2, tokens = asTokens ["ExtraBlock2"] }
                    , Line { lineIndent = 4, tokens = asTokens ["something"] }
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
                    [ Line { lineIndent = 0, tokens = asTokens ["Foo"] }
                    , Line { lineIndent = 2, tokens = [DocComment "Field"] }
                    , Line { lineIndent = 2, tokens = asTokens ["name", "String"] }
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
                [ comment
                , foo
                , name'String
                ]
                `shouldBe`
                    [ LinesWithComments
                        { lwcComments = ["comment"]
                        , lwcLines = foo :| [name'String]
                        }
                    ]
        let bar =
                Line
                    { lineIndent = 0
                    , tokens = Token "Bar" :| asTokens ["sql", "=", "bars"]
                    }
            age'Int =
                Line
                    { lineIndent = 1
                    , tokens = Token "age" :| [Token "Int"]
                    }
        it "works when used consecutively" $ do
            associateLines
                [ bar
                , age'Int
                , comment
                , foo
                , name'String
                ]
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
            let text = "Foo\n  x X\n-- | Hello\nBar\n name String"
                parsed = preparse text
                allFull = maybe [] skipEmpty parsed
            associateLines allFull
                `shouldBe`
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
            let text = maybe [] skipEmpty . preparse . T.unlines $
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
            associateLines text `shouldBe`
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
            let text = maybe [] skipEmpty . preparse . T.unlines $
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
            associateLines text `shouldBe`
                [ LinesWithComments
                    { lwcLines = Line 0 (pure (Token "IdTable")) :|
                        [ Line 4 (Token "Id" :| asTokens ["Day", "default=CURRENT_DATE"])
                        , Line 4 (Token "name" :| asTokens ["Text"])
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
            let text = maybe [] skipEmpty . preparse . T.unlines $
                    [ "-- | Model"
                    , "Foo"
                    , "  -- | Field"
                    , "  name String"
                    ]
            associateLines text `shouldBe`
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
            entityHaskell subject `shouldBe` EntityNameHS "Foo"
        describe "entityFields" $ do
            let fields = entityFields subject
            it "has the right field names" $ do
                map fieldHaskell fields `shouldMatchList`
                    [ FieldNameHS "name"
                    , FieldNameHS "age"
                    ]
            it "has comments" $ do
                map fieldComments fields `shouldBe`
                    [ Just "Field\n"
                    , Nothing
                    ]
        it "has the comments" $ do
            entityComments subject `shouldBe`
                Just "Comment\n"
        it "combines extrablocks" $ do
            entityExtra subject `shouldBe` Map.fromList
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
                                [a, b, c]
                            xs ->
                                error
                                $ "Expected 3 elements in list, got: "
                                <> show (length xs)
                                <> ", list contents: \n\n" <> intercalate "\n" (map show xs)
            describe "idTable" $ do
                let EntityDef {..} = idTable
                it "has no extra blocks" $ do
                    entityExtra `shouldBe` mempty
                it "has the right name" $ do
                    entityHaskell `shouldBe` EntityNameHS "IdTable"
                it "has the right fields" $ do
                    map fieldHaskell entityFields `shouldMatchList`
                        [ FieldNameHS "name"
                        ]
            describe "lowerCaseTable" $ do
                let EntityDef {..} = lowerCaseTable
                it "has the right name" $ do
                    entityHaskell `shouldBe` EntityNameHS "LowerCaseTable"
                it "has the right fields" $ do
                    map fieldHaskell entityFields `shouldMatchList`
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

    describe "fromPersistValue" $
        describe "UTCTime" $
            it "works with format" $
                fromPersistValue (PersistText "2018-02-27 10:49:42.123")
                    `shouldBe` Right (UTCTime (fromGregorian 2018 02 27) (timeOfDayToTime (TimeOfDay 10 49 42.123)))

asTokens :: [T.Text] -> [Token]
asTokens = fmap Token

arbitraryWhiteSpaceChar :: Gen Char
arbitraryWhiteSpaceChar =
  oneof $ pure <$> [' ', '\t', '\n', '\r']
