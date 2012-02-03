{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit

import Database.Persist.Quasi

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
