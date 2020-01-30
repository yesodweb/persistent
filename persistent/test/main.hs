import Test.Hspec
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL

import Database.Persist.Quasi
import Database.Persist.Types

main :: IO ()
main = hspec $ do
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
    describe "parseLines" $ do
        let subject = parse lowerCaseSettings
        let lines =
                T.unlines
                    [ "-- | Comment"
                    , "Foo"
                    , "  name String"
                    ]
        it "works" $ do
            subject lines `shouldBe`
                [ EntityDef
                    { entityHaskell = HaskellName "Foo"
                    , entityDB = DBName "foo"
                    , entityId =
                        FieldDef
                            { fieldHaskell = HaskellName "Id"
                            , fieldDB = DBName "id"
                            , fieldType = FTTypeCon Nothing "FooId"
                            , fieldSqlType = SqlInt64
                            , fieldAttrs = []
                            , fieldStrict = True
                            , fieldReference =
                                ForeignRef
                                    (HaskellName "Foo")
                                    (FTTypeCon (Just "Data.Int") "Int64")
                            , fieldComments = Nothing
                            }
                    , entityFields =
                        [ FieldDef
                            { fieldHaskell = HaskellName "name"
                            , fieldDB = DBName "name"
                            , fieldType = FTTypeCon Nothing "String"
                            , fieldSqlType = SqlOther "SqlType unset for name"
                            , fieldAttrs = []
                            , fieldStrict = True
                            , fieldReference = NoReference
                            , fieldComments = Nothing
                            }
                        ]
                    , entityUniques = []
                    , entityForeigns = []
                    , entityDerives = []
                    , entityAttrs = []
                    , entityExtra = mempty
                    , entitySum = False
                    , entityComments = Just "Comment\n"
                    }
                ]
