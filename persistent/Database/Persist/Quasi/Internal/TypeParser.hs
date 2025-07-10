{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Quasi.Internal.TypeParser
    ( TypeExpr (..)
    , TypeConstructor (..)
    , typeExpr
    , innerTypeExpr
    , typeExprContent
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | A parsed type expression.
--
-- @since 2.17.1.0
data TypeExpr
    = TypeApplication TypeExpr [TypeExpr]
    | TypeConstructorExpr TypeConstructor
    | TypeLitString String
    | TypeLitInt String
    | TypeLitPromotedConstructor TypeConstructor
    deriving (Show, Eq)

-- | A parsed type constructor.
--
-- @since 2.17.1.0
data TypeConstructor
    = ListConstructor
    | TypeConstructor String
    deriving (Show, Eq)

-- | Parses a Persistent-style type expression.
-- Persistent's type expressions are largely similar to Haskell's, but with a few differences:
--
-- 1. Syntactic sugar is not currently supported for constructing types other than List.
-- 2. Only certain typelevel literals are supported: Strings, Ints, and promoted type constructors.
-- 3. Because they must be parsed as part of an entity field definition, top-level applications
--    of non-nullary type constructors (except for the sugary List constructor) must
--    be parenthesized.
--
--    VALID:    Int
--    VALID:    [Maybe Int]
--    VALID:    (Maybe Int)
--    INVALID:  Maybe Int
--
-- @since 2.17.1.0
typeExpr :: ((MonadParsec e String) m) => m TypeExpr
typeExpr = typeExpr' Outer

-- | Parses a type expression in non-top-level contexts, where an unparenthesized type constructor
-- application is acceptable.
--
-- @since 2.17.1.0
innerTypeExpr :: ((MonadParsec e String) m) => m TypeExpr
innerTypeExpr = typeExpr' Inner

data IsInner = Inner | Outer

typeExpr' :: ((MonadParsec e String) m) => IsInner -> m TypeExpr
typeExpr' isInner = label "type expression" $ do
    let
        validEmbeddedApplications = case isInner of
            Inner ->
                [ simpleTypeApplication
                , complexTypeApplication
                ]
            Outer -> [nullaryTypeApplication]
    choice $
        validEmbeddedApplications
            ++ [ whitespaceBetween '(' ')' innerTypeExpr
               , listType
               , typeLitPromotedConstructor
               , typeLitString
               , typeLitInt
               ]
  where
    -- This is a proper subset of `simpleTypeApplication`.
    nullaryTypeApplication :: ((MonadParsec e String) m) => m TypeExpr
    nullaryTypeApplication = do
        tc <- typeConstructor <* optional hspace
        pure $ TypeApplication (TypeConstructorExpr tc) []

-- This does not parse sugary constructors such as the List constructor `[]`.
typeConstructor :: ((MonadParsec e String) m) => m TypeConstructor
typeConstructor = do
    first <- upperChar
    rest <- many $ choice [alphaNumChar, char '.', char '\'']
    pure $ TypeConstructor (first : rest)

whitespaceBetween :: ((MonadParsec e String) m) => Char -> Char -> m a -> m a
whitespaceBetween ldelim rdelim =
    between (char ldelim *> optional hspace) (optional hspace *> char rdelim)

complexTypeApplication :: ((MonadParsec e String) m) => m TypeExpr
complexTypeApplication = do
    t <- whitespaceBetween '(' ')' innerTypeExpr <* hspace
    args <- some (typeExpr <* optional hspace)
    pure $ TypeApplication t args

simpleTypeApplication :: ((MonadParsec e String) m) => m TypeExpr
simpleTypeApplication = do
    tc <- typeConstructor <* optional hspace
    args <- many (typeExpr <* optional hspace)
    pure $ TypeApplication (TypeConstructorExpr tc) args

typeLitString :: ((MonadParsec e String) m) => m TypeExpr
typeLitString = do
    s <- char '"' *> manyTill L.charLiteral (char '"')
    pure $ TypeLitString s

typeLitInt :: ((MonadParsec e String) m) => m TypeExpr
typeLitInt = TypeLitInt <$> some digitChar

typeLitPromotedConstructor :: ((MonadParsec e String) m) => m TypeExpr
typeLitPromotedConstructor = do
    _ <- char '\'' <* optional hspace
    TypeLitPromotedConstructor <$> typeConstructor

listType :: ((MonadParsec e String) m) => m TypeExpr
listType = do
    t <- whitespaceBetween '[' ']' innerTypeExpr
    pure $ TypeApplication (TypeConstructorExpr ListConstructor) [t]

-- | Given a TypeExpr, renders it back to a String in a canonical form that looks
-- normal to humans and is re-parseable when making an UnboundEntityDef that uses it.
--
-- @since 2.17.1.0
typeExprContent :: TypeExpr -> Text
typeExprContent = typeExprContent' Outer

-- This is a little gnarly-looking. That's mostly due to attempting to avoid inserting
-- superfluous parentheses.
typeExprContent' :: IsInner -> TypeExpr -> Text
typeExprContent' isInner = \case
    TypeLitString s ->
        mconcat
            [ "\""
            , T.pack s
            , "\""
            ]
    TypeLitInt s -> T.pack s
    TypeLitPromotedConstructor tc -> "'" <> typeExprContent' isInner (TypeConstructorExpr tc)
    TypeConstructorExpr (TypeConstructor s) -> T.pack s
    TypeConstructorExpr ListConstructor -> "List"
    TypeApplication (TypeConstructorExpr tc) args -> simpleTypeApplicationContent tc args isInner
    TypeApplication t exps ->
        mconcat
            [ typeExprContent' Inner t
            , " "
            , T.intercalate " " $ fmap typeExprContent exps
            ]
  where
    typeArgsListContent :: IsInner -> [TypeExpr] -> Text
    typeArgsListContent i exps = T.intercalate " " $ fmap (typeExprContent' i) exps

    simpleTypeApplicationContent :: TypeConstructor -> [TypeExpr] -> IsInner -> Text
    simpleTypeApplicationContent ListConstructor args _ =
        mconcat
            [ "["
            , typeArgsListContent Outer args
            , "]"
            ]
    simpleTypeApplicationContent (TypeConstructor s) [] _ = T.pack s
    simpleTypeApplicationContent (TypeConstructor s) exps Inner =
        mconcat
            [ "("
            , simpleTypeApplicationContent (TypeConstructor s) exps Outer
            , ")"
            ]
    simpleTypeApplicationContent (TypeConstructor s) exps Outer =
        mconcat
            [ T.pack s
            , " "
            , typeArgsListContent Inner exps
            ]
