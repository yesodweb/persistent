{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit

import Database.Persist.Quasi
import Database.Persist.EntityDef

main :: IO ()
main = hspecX $ do
    describe "tokenization" $ do
        it "handles normal words" $
            tokenize " foo   bar  baz" @?=
                [ Spaces 1
                , Token "foo"
                , Spaces 3
                , Token "bar"
                , Spaces 2
                , Token "baz"
                ]
        it "handles quotes" $
            tokenize "  \"foo bar\"  \"baz\"" @?=
                [ Spaces 2
                , Token "foo bar"
                , Spaces 2
                , Token "baz"
                ]
        it "handles unnested parantheses" $
            tokenize "  (foo bar)  (baz)" @?=
                [ Spaces 2
                , Token "foo bar"
                , Spaces 2
                , Token "baz"
                ]
        it "handles nested parantheses" $
            tokenize "  (foo (bar))  (baz)" @?=
                [ Spaces 2
                , Token "foo (bar)"
                , Spaces 2
                , Token "baz"
                ]
        it "escaping " $
            tokenize "  (foo \\(bar)  \"baz\\\"\"" @?=
                [ Spaces 2
                , Token "foo (bar"
                , Spaces 2
                , Token "baz\""
                ]
    describe "parseFieldType" $ do
        it "simple types" $
            parseFieldType "FooBar" @?= Just (FTTypeCon Nothing "FooBar")
        it "module types" $
            parseFieldType "Data.Map.FooBar" @?= Just (FTTypeCon (Just "Data.Map") "FooBar")
        it "application" $
            parseFieldType "Foo Bar" @?= Just (
                FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
        it "application multiple" $
            parseFieldType "Foo Bar Baz" @?= Just (
                (FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
                `FTApp` FTTypeCon Nothing "Baz"
                )
        it "parens" $ do
            let foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo (Bar Baz)" @?= Just (
                foo `FTApp` (bar `FTApp` baz))
        it "lists" $ do
            let foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                bars = FTList bar
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo [Bar] Baz" @?= Just (
                foo `FTApp` bars `FTApp` baz)
    describe "stripId" $ do
        it "works" $
            (parseFieldType "FooId" >>= stripId) @?= Just "Foo"
