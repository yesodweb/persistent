{-# LANGUAGE OverloadedStrings #-}
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
        it "handles unnested parantheses" $
            tokenize "  (foo bar)  (baz)" `shouldBe`
                [ Spaces 2
                , Token "foo bar"
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
        it "escaping " $
            tokenize "  (foo \\(bar)  \"baz\\\"\"" `shouldBe`
                [ Spaces 2
                , Token "foo (bar"
                , Spaces 2
                , Token "baz\""
                ]
    describe "parseFieldType" $ do
        it "simple types" $
            parseFieldType "FooBar" `shouldBe` Just (FTTypeCon Nothing "FooBar")
        it "module types" $
            parseFieldType "Data.Map.FooBar" `shouldBe` Just (FTTypeCon (Just "Data.Map") "FooBar")
        it "application" $
            parseFieldType "Foo Bar" `shouldBe` Just (
                FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
        it "application multiple" $
            parseFieldType "Foo Bar Baz" `shouldBe` Just (
                (FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
                `FTApp` FTTypeCon Nothing "Baz"
                )
        it "parens" $ do
            let foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo (Bar Baz)" `shouldBe` Just (
                foo `FTApp` (bar `FTApp` baz))
        it "lists" $ do
            let foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                bars = FTList bar
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo [Bar] Baz" `shouldBe` Just (
                foo `FTApp` bars `FTApp` baz)
    describe "stripId" $ do
        it "works" $
            (parseFieldType "FooId" >>= stripId) `shouldBe` Just "Foo"
