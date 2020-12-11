{-# language RecordWildCards, OverloadedStrings, QuasiQuotes #-}

import Test.Hspec
import qualified Data.Char as Char
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import Data.Time
import Text.Shakespeare.Text
import Data.List

import Database.Persist.Class.PersistField
import Database.Persist.Quasi
import Database.Persist.Types

main :: IO ()
main = hspec $ do
    describe "splitExtras" $ do
        it "works" $ do
            splitExtras []
                `shouldBe`
                    mempty
        it "works2" $ do
            splitExtras
                [ Line 0 ["hello", "world"]
                ]
                `shouldBe`
                    ( [["hello", "world"]], mempty )
        it "works3" $ do
            splitExtras
                [ Line 0 ["hello", "world"]
                , Line 2 ["foo", "bar", "baz"]
                ]
                `shouldBe`
                    ( [["hello", "world"], ["foo", "bar", "baz"]], mempty )
        it "works4" $ do
            let foobarbarz = ["foo", "Bar", "baz"]
            splitExtras
                [ Line 0 ["Hello"]
                , Line 2 foobarbarz
                , Line 2 foobarbarz
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
                [ Line 0 ["Hello"]
                , Line 2 foobarbarz
                , Line 4 foobarbarz
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
                        { fieldHaskell = HaskellName "asdf"
                        , fieldDB = DBName "asdf"
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
                        { fieldHaskell = HaskellName "asdf"
                        , fieldDB = DBName "asdf"
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
                        { fieldHaskell = HaskellName "asdf"
                        , fieldDB = DBName "asdf"
                        , fieldType = FTTypeCon Nothing "UserId"
                        , fieldSqlType = SqlOther "SqlType unset for asdf"
                        , fieldAttrs = []
                        , fieldStrict = True
                        , fieldReference = NoReference
                        , fieldCascade = FieldCascade Nothing (Just Cascade)
                        , fieldComments = Nothing
                        , fieldGenerated = Nothing
                        }

    describe "tokenization" $ do
        it "handles normal words" $
            tokenize " foo   bar  baz" `shouldBe`
                [ Spaces 1
                , Token "foo"
                , Spaces 3
                , Token "bar"
                , Spaces 2
                , Token "baz"
                ]
        it "handles quotes" $
            tokenize "  \"foo bar\"  \"baz\"" `shouldBe`
                [ Spaces 2
                , Token "foo bar"
                , Spaces 2
                , Token "baz"
                ]
        it "handles quotes mid-token" $
            tokenize "  x=\"foo bar\"  \"baz\"" `shouldBe`
                [ Spaces 2
                , Token "x=foo bar"
                , Spaces 2
                , Token "baz"
                ]
        it "handles escaped quote mid-token" $
            tokenize "  x=\\\"foo bar\"  \"baz\"" `shouldBe`
                [ Spaces 2
                , Token "x=\\\"foo"
                , Spaces 1
                , Token "bar\""
                , Spaces 2
                , Token "baz"
                ]
        it "handles unnested parantheses" $
            tokenize "  (foo bar)  (baz)" `shouldBe`
                [ Spaces 2
                , Token "foo bar"
                , Spaces 2
                , Token "baz"
                ]
        it "handles unnested parantheses mid-token" $
            tokenize "  x=(foo bar)  (baz)" `shouldBe`
                [ Spaces 2
                , Token "x=foo bar"
                , Spaces 2
                , Token "baz"
                ]
        it "handles nested parantheses" $
            tokenize "  (foo (bar))  (baz)" `shouldBe`
                [ Spaces 2
                , Token "foo (bar)"
                , Spaces 2
                , Token "baz"
                ]
        it "escaping" $
            tokenize "  (foo \\(bar)  y=\"baz\\\"\"" `shouldBe`
                [ Spaces 2
                , Token "foo (bar"
                , Spaces 2
                , Token "y=baz\""
                ]
        it "mid-token quote in later token" $
            tokenize "foo bar baz=(bin\")" `shouldBe`
                [ Token "foo"
                , Spaces 1
                , Token "bar"
                , Spaces 1
                , Token "baz=bin\""
                ]
        describe "comments" $ do
            it "recognizes one line" $ do
                tokenize "-- | this is a comment" `shouldBe`
                    [ DocComment "-- | this is a comment"
                    ]
            it "map tokenize" $ do
                map tokenize ["Foo", "-- | Hello"]
                    `shouldBe`
                        [ [Token "Foo"]
                        , [DocComment "-- | Hello"]
                        ]
            it "works if comment is indented" $ do
                tokenize "  -- | comment" `shouldBe`
                    [ Spaces 2, DocComment "-- | comment"
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
            length preparsed
                `shouldBe` do
                    length . filter (not . T.all Char.isSpace) . T.lines
                        $ subject

        let skippedEmpty =
                skipEmpty preparsed
            fooLines =
                [ Line
                    { lineIndent = 0
                    , tokens = "Foo" :| []
                    }
                , Line
                    { lineIndent = 4
                    , tokens = "name" :| ["String"]
                    }
                , Line
                    { lineIndent = 4
                    , tokens = "age" :| ["Int"]
                    }
                ]
            emptyLines =
                [ Line
                    { lineIndent = 0
                    , tokens = "EmptyEntity" :| []
                    }
                ]
            barLines =
                [ Line
                    { lineIndent = 0
                    , tokens = "Bar" :| []
                    }
                , Line
                    { lineIndent = 4
                    , tokens = "name" :| ["String"]
                    }
                ]
            bazLines =
                [ Line
                    { lineIndent = 0
                    , tokens = "Baz" :| []
                    }
                , Line
                    { lineIndent = 4
                    , tokens = "a" :| ["Int"]
                    }
                , Line
                    { lineIndent = 4
                    , tokens = "b" :| ["String"]
                    }
                , Line
                    { lineIndent = 4
                    , tokens = "c" :| ["FooId"]
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
                        (((name, fieldCount) :_), []) ->
                            expectationFailure
                                $ "Expected an entity with name "
                                <> name
                                <> " and " <> show fieldCount <> " fields"
                                <> ", but the list was empty..."
                        ((name, fieldCount) : ys, (EntityDef {..} : xs)) -> do
                            (unHaskellName entityHaskell, length entityFields)
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
        it "recognizes entity" $ do
            preparse "Person\n  name String\n  age Int" `shouldBe`
                [ Line { lineIndent = 0, tokens = ["Person"] }
                , Line { lineIndent = 2, tokens = ["name", "String"] }
                , Line { lineIndent = 2, tokens = ["age", "Int"] }
                ]
        describe "recognizes comments" $ do
            let text = "Foo\n  x X\n-- | Hello\nBar\n name String"
                linesText = T.lines text
            it "T.lines" $ do
                linesText
                    `shouldBe`
                        [ "Foo"
                        , "  x X"
                        , "-- | Hello"
                        , "Bar"
                        , " name String"
                        ]
            let tokens = map tokenize linesText
            it "map tokenize" $ do
                tokens `shouldBe`
                    [ [ Token "Foo" ]
                    , [ Spaces 2, Token "x", Spaces 1, Token "X"]
                    , [ DocComment "-- | Hello" ]
                    , [ Token "Bar" ]
                    , [ Spaces 1, Token "name", Spaces 1, Token "String" ]
                    ]
            let filtered = filter (not . empty) tokens
            it "filter (not . empty)" $ do
                filtered `shouldBe`
                    [ [ Token "Foo" ]
                    , [ Spaces 2, Token "x", Spaces 1, Token "X"]
                    , [ DocComment "-- | Hello" ]
                    , [ Token "Bar" ]
                    , [ Spaces 1, Token "name", Spaces 1, Token "String" ]
                    ]
            let spacesRemoved = removeSpaces filtered
            it "removeSpaces" $ do
                spacesRemoved `shouldBe`
                    [ Line { lineIndent = 0, tokens = ["Foo"] }
                    , Line { lineIndent = 2, tokens = ["x", "X"] }
                    , Line { lineIndent = 0, tokens = ["-- | Hello"] }
                    , Line { lineIndent = 0, tokens = ["Bar"] }
                    , Line { lineIndent = 1, tokens = ["name", "String"] }
                    ]

            it "preparse" $ do
                preparse text `shouldBe`
                    [ Line { lineIndent = 0, tokens = ["Foo"] }
                    , Line { lineIndent = 2, tokens = ["x", "X"] }
                    , Line { lineIndent = 0, tokens = ["-- | Hello"] }
                    , Line { lineIndent = 0, tokens = ["Bar"] }
                    , Line { lineIndent = 1, tokens = ["name", "String"] }
                    ]
            it "preparse indented" $ do
                let t = T.unlines
                        [ "  Foo"
                        , "    x X"
                        , "  -- | Comment"
                        , "  -- hidden comment"
                        , "  Bar"
                        , "    name String"
                        ]
                preparse t `shouldBe`
                    [ Line { lineIndent = 2, tokens = ["Foo"] }
                    , Line { lineIndent = 4, tokens = ["x", "X"] }
                    , Line { lineIndent = 2, tokens = ["-- | Comment"] }
                    , Line { lineIndent = 2, tokens = ["Bar"] }
                    , Line { lineIndent = 4, tokens = ["name", "String"] }
                    ]
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
                preparse t `shouldBe`
                    [ Line { lineIndent = 0, tokens = ["LowerCaseTable"] }
                    , Line { lineIndent = 2, tokens = ["name", "String"] }
                    , Line { lineIndent = 2, tokens = ["ExtraBlock"] }
                    , Line { lineIndent = 4, tokens = ["foo", "bar"] }
                    , Line { lineIndent = 4, tokens = ["baz"] }
                    , Line { lineIndent = 2, tokens = ["ExtraBlock2"] }
                    , Line { lineIndent = 4, tokens = ["something"] }
                    ]
            it "field comments" $ do
                let text = T.unlines
                        [ "-- | Model"
                        , "Foo"
                        , "  -- | Field"
                        , "  name String"
                        ]
                preparse text `shouldBe`
                    [ Line { lineIndent = 0, tokens = ["-- | Model"] }
                    , Line { lineIndent = 0, tokens = ["Foo"] }
                    , Line { lineIndent = 2, tokens = ["-- | Field"] }
                    , Line { lineIndent = 2, tokens = ["name", "String"] }
                    ]

    describe "empty" $ do
        it "doesn't dispatch comments" $ do
            [DocComment "-- | hello"] `shouldSatisfy` (not . empty)
        it "removes spaces" $ do
            [Spaces 3] `shouldSatisfy` empty

    describe "filter (not . empty)" $ do
        let subject = filter (not . empty)
        it "keeps comments" $ do
            subject [[DocComment "-- | Hello"]]
                `shouldBe`
                    [[DocComment "-- | Hello"]]
        it "omits lines with only spaces" $ do
            subject [[Spaces 3, Token "indented"], [Spaces 2]]
                `shouldBe`
                    [[Spaces 3, Token "indented"]]

    describe "removeSpaces" $ do
        it "sets indentation level for a line" $ do
            removeSpaces [[Spaces 3, Token "hello", Spaces 1, Token "goodbye"]]
                `shouldBe`
                    [ Line { lineIndent = 3, tokens = ["hello", "goodbye"] }
                    ]
        it "does not remove comments" $ do
            removeSpaces
                [ [ DocComment "-- | asdf" ]
                , [ Token "Foo" ]
                , [ Spaces 2, Token "name", Spaces 1, Token "String" ]
                ]
                `shouldBe`
                    [ Line { lineIndent = 0, tokens = ["-- | asdf"] }
                    , Line { lineIndent = 0, tokens = ["Foo"] }
                    , Line { lineIndent = 2, tokens = ["name", "String"] }
                    ]

    describe "associateLines" $ do
        let foo = Line { lineIndent = 0, tokens = pure "Foo" }
            name'String = Line { lineIndent = 2, tokens = "name" :| ["String"] }
            comment = Line { lineIndent = 0, tokens = pure "-- | comment" }
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
        let bar = Line { lineIndent = 0, tokens = "Bar" :| ["sql", "=", "bars"] }
            age'Int = Line { lineIndent = 1, tokens = "age" :| ["Int"] }
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
                allFull = skipEmpty parsed
            associateLines allFull
                `shouldBe`
                    [ LinesWithComments
                        { lwcLines =
                            Line {lineIndent = 0, tokens = "Foo" :| []}
                            :| [ Line {lineIndent = 2, tokens = "x" :| ["X"]} ]
                        , lwcComments =
                            []
                        }
                    , LinesWithComments
                        { lwcLines =
                            Line {lineIndent = 0, tokens = "Bar" :| []}
                            :| [ Line {lineIndent = 1, tokens = "name" :| ["String"]}]
                        , lwcComments =
                            ["Hello"]
                        }
                    ]
        it "works with extra blocks" $ do
            let text = skipEmpty . preparse . T.unlines $
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
                        Line { lineIndent = 0, tokens = pure "LowerCaseTable" } :|
                        [ Line { lineIndent = 4, tokens = "Id" :| ["sql=my_id"] }
                        , Line { lineIndent = 4, tokens = "fullName" :| ["Text"] }
                        , Line { lineIndent = 4, tokens = pure "ExtraBlock" }
                        , Line { lineIndent = 8, tokens = "foo" :| ["bar"] }
                        , Line { lineIndent = 8, tokens = pure "baz" }
                        , Line { lineIndent = 8, tokens = pure "bin" }
                        , Line { lineIndent = 4, tokens = pure "ExtraBlock2" }
                        , Line { lineIndent = 8, tokens = pure "something" }
                        ]
                    , lwcComments = []
                    }
                ]

        it "works with extra blocks twice" $ do
            let text = skipEmpty . preparse . T.unlines $
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
                    { lwcLines = Line 0 (pure "IdTable") :|
                        [ Line 4 ("Id" :| ["Day", "default=CURRENT_DATE"])
                        , Line 4 ("name" :| ["Text"])
                        ]
                    , lwcComments = []
                    }
                , LinesWithComments
                    { lwcLines =
                        Line { lineIndent = 0, tokens = pure "LowerCaseTable" } :|
                        [ Line { lineIndent = 4, tokens = "Id" :| ["sql=my_id"] }
                        , Line { lineIndent = 4, tokens = "fullName" :| ["Text"] }
                        , Line { lineIndent = 4, tokens = pure "ExtraBlock" }
                        , Line { lineIndent = 8, tokens = "foo" :| ["bar"] }
                        , Line { lineIndent = 8, tokens = pure "baz" }
                        , Line { lineIndent = 8, tokens = pure "bin" }
                        , Line { lineIndent = 4, tokens = pure "ExtraBlock2" }
                        , Line { lineIndent = 8, tokens = pure "something" }
                        ]
                    , lwcComments = []
                    }
                ]


        it "works with field comments" $ do
            let text = skipEmpty . preparse . T.unlines $
                    [ "-- | Model"
                    , "Foo"
                    , "  -- | Field"
                    , "  name String"
                    ]
            associateLines text `shouldBe`
                [ LinesWithComments
                    { lwcLines =
                        Line { lineIndent = 0, tokens = "Foo" :| [] } :|
                            [ Line { lineIndent = 2, tokens = pure "-- | Field" }
                            , Line { lineIndent = 2, tokens = "name" :| ["String"] }
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
            entityHaskell subject `shouldBe` HaskellName "Foo"
        describe "entityFields" $ do
            let fields = entityFields subject
            it "has the right field names" $ do
                map fieldHaskell fields `shouldMatchList`
                    [ HaskellName "name"
                    , HaskellName "age"
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
                    entityHaskell `shouldBe` HaskellName "IdTable"
                it "has the right fields" $ do
                    map fieldHaskell entityFields `shouldMatchList`
                        [ HaskellName "name"
                        ]
            describe "lowerCaseTable" $ do
                let EntityDef {..} = lowerCaseTable
                it "has the right name" $ do
                    entityHaskell `shouldBe` HaskellName "LowerCaseTable"
                it "has the right fields" $ do
                    map fieldHaskell entityFields `shouldMatchList`
                        [ HaskellName "fullName"
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
