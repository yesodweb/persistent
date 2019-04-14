import Test.Hspec

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
