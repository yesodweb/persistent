{-|
Module: module Database.Persist.Sql.Raw.QQ
Description: QuasiQuoters for performing raw sql queries

This module exports convenient QuasiQuoters to perform raw SQL queries.
All QuasiQuoters follow them same pattern and are analogous to the similar named
functions exported from 'Database.Persist.Sql.Raw'. Neither the quoted
function's behaviour nor it's return value is altered during the translation and
all documentation provided with these functions holds.

The QuasiQuoters in this module perform a simple substitution on the query text,
where interpolated values of the form @#{foo}@ are replaced with a single
question mark ('?') and collected into a list. The targeted function is then
applied to the modified query text as well as a list of all found values.
Please note that it is not required to call 'toPersistValue' on each
interpolated value, as this conversion is done automatically.

Here is a small example:

@
   let lft = 10 :: Int
       rgt = 20 :: Int
       width = 50 :: Int
   in [sqlQQ|
           DELETE FROM category WHERE lft BETWEEN #{lft} AND #{rgt};
           UPDATE category SET rgt = rgt - #{width} WHERE rgt > #{rgt};
           UPDATE category SET lft = lft - #{width} WHERE lft > #{rgt};
           |]
@

This directly translates to this:

@
   let lft = 10 :: Int
       rgt = 20 :: Int
       width = 50 :: Int
   in rawSql (
        "DELETE FROM category WHERE lft BETWEEN ? AND ?;"  <>
        "UPDATE category SET rgt = rgt - ? WHERE rgt > ?;" <>
        "UPDATE category SET lft = lft - ? WHERE lft > ?;"
        ) [ toPersistValue lft
          , toPersistValue rgt
          , toPersistValue width
          , toPersistValue rgt
          , toPersistValue width
          , toPersistValue rgt
          ]
@

-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Sql.Raw.QQ (
      -- * Sql QuasiQuoters
      queryQQ
    , queryResQQ
    , sqlQQ
    , executeQQ
    , executeCountQQ
    ) where

import Control.Arrow (first, second)
import Data.Text (pack)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import Database.Persist.Class (toPersistValue)
import Database.Persist.Sql.Raw (rawSql, rawQuery, rawQueryRes, rawExecute, rawExecuteCount)

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

makeExpr :: TH.Q TH.Exp -> [StringPart] -> TH.ExpQ
makeExpr f s = TH.appE [| uncurry $(f) . first pack |] (go s)
    where
    go [] = [| (mempty, []) |]
    go (Literal a:xs)   = TH.appE [| first (a ++) |] (go xs)
    go (AntiQuote a:xs) = TH.appE [| first ("?" ++) . second (toPersistValue $(reify a) :) |] (go xs)

reify :: String -> TH.Q TH.Exp
reify s =
    case parseExp s of
        Left e -> TH.reportError e >> [| mempty |]
        Right v -> return v

makeQQ :: TH.Q TH.Exp -> QuasiQuoter
makeQQ x = QuasiQuoter
    (makeExpr x . parseStr [])
    (error "Cannot use qc as a pattern")
    (error "Cannot use qc as a type")
    (error "Cannot use qc as a dec")

-- | Analoguous to 'Database.Persist.Sql.Raw.rawSql'
-- @since 2.7.2
sqlQQ :: QuasiQuoter
sqlQQ = makeQQ [| rawSql |]

-- | Analoguous to 'Database.Persist.Sql.Raw.rawExecute'
-- @since 2.7.2
executeQQ :: QuasiQuoter
executeQQ = makeQQ [| rawExecute |]

-- | Analoguous to 'Database.Persist.Sql.Raw.rawExecuteCount'
-- @since 2.7.2
executeCountQQ :: QuasiQuoter
executeCountQQ = makeQQ [| rawExecuteCount |]

-- | Analoguous to 'Database.Persist.Sql.Raw.rawQuery'
-- @since 2.7.2
queryQQ :: QuasiQuoter
queryQQ = makeQQ [| rawQuery |]

-- | Analoguous to 'Database.Persist.Sql.Raw.rawQueryRes'
-- @since 2.7.2
queryResQQ :: QuasiQuoter
queryResQQ = makeQQ [| rawQueryRes |]
