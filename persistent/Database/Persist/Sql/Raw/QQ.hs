{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Sql.Raw.QQ (
      sqlQQ
    , executeQQ
    ) where

import Control.Arrow (first, second)
import Data.Text (pack)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import Database.Persist.Class (toPersistValue)
import qualified Database.Persist.Sql.Raw as Raw

data StringPart
  = Literal String
  | AntiQuote String
  deriving Show

parseHaskell :: String -> String -> [StringPart]
parseHaskell a []          = [Literal (reverse a)]
parseHaskell a ('\\':x:xs) = parseHaskell (x:a) xs
parseHaskell a ['\\']      = parseHaskell ('\\':a) []
parseHaskell a ('}':xs)    = AntiQuote (reverse a) : parseStr [] xs
parseHaskell a (x:xs)      = parseHaskell (x:a) xs

parseStr :: String -> String -> [StringPart]
parseStr a []           = [Literal (reverse a)]
parseStr a ('\\':x:xs)  = parseStr (x:a) xs
parseStr a ['\\']       = parseStr ('\\':a) []
parseStr a ('#':'{':xs) = Literal (reverse a) : parseHaskell [] xs
parseStr a (x:xs)       = parseStr (x:a) xs

makeExpr :: [StringPart] -> TH.ExpQ
makeExpr s = TH.appE [| uncurry Raw.rawSql . first pack |] (go s)
    where
    go [] = [| (mempty, []) |]
    go (Literal a:xs)   = TH.appE [| first (a ++) |] (go xs)
    go (AntiQuote a:xs) = TH.appE [| first ("?" ++) . second (toPersistValue $(reify a) :) |] (go xs)

makeExpr' :: [StringPart] -> TH.ExpQ
makeExpr' s = TH.appE [| uncurry Raw.rawExecute . first pack |] (go s)
    where
    go [] = [| (mempty, []) |]
    go (Literal a:xs)   = TH.appE [| first (a ++) |] (go xs)
    go (AntiQuote a:xs) = TH.appE [| first ("?" ++) . second (toPersistValue $(reify a) :) |] (go xs)

reify :: String -> TH.Q TH.Exp
reify s =
    case parseExp s of
        Left e -> TH.reportError e >> [| mempty |]
        Right v -> return v

sqlQQ :: QuasiQuoter
sqlQQ = QuasiQuoter
    (makeExpr . parseStr [] . filter (/= '\r'))
    (error "Cannot use qc as a pattern")
    (error "Cannot use qc as a type")
    (error "Cannot use qc as a dec")

executeQQ :: QuasiQuoter
executeQQ = QuasiQuoter
    (makeExpr' . parseStr [] . filter (/= '\r'))
    (error "Cannot use qc as a pattern")
    (error "Cannot use qc as a type")
    (error "Cannot use qc as a dec")
