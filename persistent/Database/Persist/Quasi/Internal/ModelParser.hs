{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Database.Persist.Quasi.Internal.ModelParser
    ( SourceLoc (..)
    , Attribute (..)
    , attribute
    , attributeContent
    , Directive (..)
    , directiveContent
    , EntityField (..)
    , entityField
    , entityFieldContent
    , FieldName (..)
    , fieldName
    , ParsedEntityDef (..)
    , parseSource
    , memberEntityFields
    , ParserWarning
    , parserWarningMessage
    , ParseResult
    , CumulativeParseResult
    , toCumulativeParseResult
    , renderErrors
    , runConfiguredParser
    , ParserErrorLevel (..)
    , initialExtraState
    ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, void)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State
import Control.Monad.Writer
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Foldable (fold)
import Data.Functor.Identity
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Database.Persist.Quasi.Internal.TypeParser
import Database.Persist.Quasi.PersistSettings.Internal
import Database.Persist.Types
import Database.Persist.Types.SourceSpan
import Language.Haskell.TH.Syntax (Lift)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Stream as TMS

-- We'll augment the parser with extra state to accumulate comments seen during parsing.
-- Comments are lexed as whitespace, but will be used to generate documentation later.
data ExtraState = ExtraState
    { esPositionedCommentTokens :: [(SourcePos, CommentToken)]
    , esLastDocumentablePosition :: Maybe SourcePos
    }

-- @since 2.16.0.0
initialExtraState :: ExtraState
initialExtraState =
    ExtraState
        { esPositionedCommentTokens = []
        , esLastDocumentablePosition = Nothing
        }

newtype Parser a = Parser
    { unParser
        :: ReaderT
            PersistSettings
            ( StateT
                ExtraState
                ( ParsecT
                    Void
                    String
                    ( Writer
                        (Set ParserWarning)
                    )
                )
            )
            a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , Alternative
        , MonadPlus
        , MonadState ExtraState
        , MonadReader PersistSettings
        , MonadParsec Void String
        , MonadFail
        )

type EntityParseError = ParseErrorBundle String Void

-- | Result of parsing a single source text.
--
-- @since 2.16.0.0
type ParseResult a =
    (Set ParserWarning, Either (ParseErrorBundle String Void) a)

type InternalParseResult a = ParseResult (a, ExtraState)

-- | Cumulative result of parsing multiple source texts.
--
-- @since 2.16.0.0
type CumulativeParseResult a = (Set ParserWarning, Either [EntityParseError] a)

toCumulativeParseResult
    :: (Monoid a) => [ParseResult a] -> CumulativeParseResult a
toCumulativeParseResult prs = do
    let
        (warnings, eithers) = sequence prs
    case partitionEithers eithers of
        ([], results) -> (warnings, Right $ fold results)
        (errs, _) -> (warnings, Left errs)

-- | Run a parser using provided PersistSettings and ExtraState
-- @since 2.16.0.0
runConfiguredParser
    :: PersistSettings
    -> ExtraState
    -> Parser a
    -> String
    -> String
    -> InternalParseResult a
runConfiguredParser ps acc parser fp s = (warnings, either)
  where
    sm = runReaderT (unParser parser) ps
    pm = runStateT sm acc
    wm = runParserT' pm initialInternalState
    ((_is, either), warnings) = runWriter wm

    initialSourcePos =
        SourcePos
            { sourceName = fp
            , sourceLine = pos1
            , sourceColumn = pos1
            }
    initialPosState =
        PosState
            { pstateInput = s
            , pstateOffset = 0
            , pstateSourcePos = initialSourcePos
            , -- for legacy compatibility, we treat each tab as a single unit of whitespace
              pstateTabWidth = pos1
            , pstateLinePrefix = ""
            }
    initialInternalState =
        State
            { stateInput = s
            , stateOffset = 0
            , statePosState = initialPosState
            , stateParseErrors = []
            }

reportWarnings :: Set ParserWarning -> Parser ()
#if MIN_VERSION_megaparsec(9,5,0)
reportWarnings = Parser . tell
#else
reportWarnings _pw = pure ()
#endif

-- | Renders a list of EntityParseErrors as a String using `errorBundlePretty`,
-- separated by line breaks.
-- @since 2.16.0.0
renderErrors :: [EntityParseError] -> String
renderErrors errs = intercalate "\n" $ fmap errorBundlePretty errs

-- | Attempts to parse with a provided parser. If it fails with an error matching
-- the provided predicate, it registers a warning with the provided message and falls
-- back to the second provided parser.
tryOrWarn
    :: String
    -> (ParseError String Void -> Bool)
    -> Parser a
    -> Parser a
    -> Parser a
tryOrWarn msg p l r = do
    parserState <- getParserState
    withRecovery (warnAndRetry $ statePosState parserState) l
  where
    warnAndRetry posState err = do
        if p err
            then do
                let
                    (pairs, _) = attachSourcePos errorOffset [err] posState
                reportWarnings . Set.fromList $
                    map
                        ( \(e, _pos) ->
                            ParserWarning
                                { parserWarningExtraMessage = msg <> "\n"
                                , parserWarningUnderlyingError = e
                                , parserWarningPosState = posState
                                }
                        )
                        pairs
                r
            else parseError err

-- | Attempts to parse with a provided parser. If it fails with an error matching
-- the provided predicate, it registers a delayed error and falls
-- back to the second provided parser.
--
-- This is useful when registering errors in space consumers and other parsers that are called
-- with `try`, since a non-delayed error in this context will cause backtracking and not
-- get reported to the user.
tryOrRegisterError
    :: (ParseError String Void -> Bool)
    -> Parser a
    -> Parser a
    -> Parser a
tryOrRegisterError p l r = do
    parserState <- getParserState
    withRecovery (delayedError $ statePosState parserState) l
  where
    delayedError posState err = do
        if p err
            then do
                let
                    (pairs, _) = attachSourcePos errorOffset [err] posState
                registerParseError err
                r
            else parseError err

tryOrReport
    :: Maybe ParserErrorLevel
    -> String
    -> (ParseError String Void -> Bool)
    -> Parser a
    -> Parser a
    -> Parser a
tryOrReport level msg p l r = case level of
    Just LevelError -> tryOrRegisterError p l r
    Just LevelWarning -> tryOrWarn msg p l r
    Nothing -> r

-- | Source location: file and line/col information. This is half of a 'SourceSpan'.
--
-- @since 2.16.0.0
data SourceLoc = SourceLoc
    { locFile :: Text
    , locStartLine :: Int
    , locStartCol :: Int
    }
    deriving (Show, Lift)

-- | An attribute of an entity field definition or a directive.
--
-- @since 2.17.1.0
data Attribute
    = Assignment Text Text
    | Parenthetical Text
    | PText Text
    | -- | Quoted field attributes are deprecated since 2.17.1.0.
      Quotation Text
    deriving (Eq, Ord, Show)

-- | The name of an entity block or extra block.
--
-- @since 2.17.1.0
newtype BlockKey = BlockKey Text
    deriving (Show)

-- | A parsed comment or doc comment.
--
-- @since 2.16.0.0
data CommentToken
    = DocComment Text
    | Comment Text
    deriving (Eq, Ord, Show)

-- | Converts an attribute into a Text representation for second-stage parsing or
-- presentation to the user
--
-- @since 2.16.0.0
attributeContent :: Attribute -> Text
attributeContent = \case
    Assignment l r -> mconcat [l, "=", r]
    Parenthetical s -> s
    PText s -> s
    Quotation s -> s

-- | Converts a directive into a Text representation for second-stage parsing or
-- presentation to the user
--
-- @since 2.17.1.0
directiveContent :: Directive -> [Text]
directiveContent d =
    [directiveNameContent $ directiveName d]
        <> (attributeContent <$> directiveAttributes d)

entityFieldContent :: EntityField -> [Text]
entityFieldContent f =
    [ fieldNameAndStrictnessAsText f
    , (typeExprContent . entityFieldType) f
    ]
        ++ fmap attributeContent (entityFieldAttributes f)

blockKeyContent :: BlockKey -> Text
blockKeyContent (BlockKey t) = t

directiveNameContent :: DirectiveName -> Text
directiveNameContent (DirectiveName t) = t

-- | Generates the field name of an EntityField, accompanied by
-- its strictness sigil, if one is present.
-- This is only needed temporarily, and can eventually be refactored away.
--
-- @since 2.17.1.0
fieldNameAndStrictnessAsText :: EntityField -> Text
fieldNameAndStrictnessAsText f =
    let
        s = case entityFieldStrictness f of
            Just Strict -> "!"
            Just Lazy -> "~"
            Nothing -> ""
        (FieldName fn) = entityFieldName f
     in
        s <> fn

commentContent :: CommentToken -> Text
commentContent = \case
    Comment s -> s
    DocComment s -> s

quotedAttributeErrorMessage :: String
quotedAttributeErrorMessage = "Unexpected quotation mark in field or directive attribute"

attribute :: Parser Attribute
attribute = do
    quotedFieldAttributeErrorLevel <- asks psQuotedArgumentErrorLevel
    tryOrReport
        quotedFieldAttributeErrorLevel
        "Quoted field attributes are deprecated since 2.17.1.0, and will be removed in or after 2.18.0.0"
        isQuotedAttributeError
        attribute'
        (Quotation . Text.pack <$> quotation)
  where
    isQuotedAttributeError (FancyError _ s) = s == Set.singleton (ErrorFail quotedAttributeErrorMessage)
    isQuotedAttributeError _ = False

attribute' :: Parser Attribute
attribute' = do
    q <- lookAhead (optional $ char '"')
    case q of
        Just _ -> fail quotedAttributeErrorMessage
        Nothing ->
            choice
                [ try assignment
                , parenthetical
                , ptext
                ]

docComment :: Parser (SourcePos, CommentToken)
docComment = do
    pos <- getSourcePos
    content <-
        string "-- |" *> validHSpace *> takeWhileP (Just "character") (/= '\n')
    pure (pos, DocComment (Text.pack content))

comment :: Parser (SourcePos, CommentToken)
comment = do
    pos <- getSourcePos
    content <-
        (string "--" <|> string "#")
            *> validHSpace
            *> takeWhileP (Just "character") (/= '\n')
    pure (pos, Comment (Text.pack content))

skipComment :: Parser ()
skipComment = do
    content <- docComment <|> comment
    void $ appendCommentToState content

isValidHSpace :: Bool -> Char -> Bool
isValidHSpace allowTabs c =
    if allowTabs
        then isSpace c && c /= '\n'
        else isSpace c && c /= '\n' && c /= '\t'

isValidSpace :: Bool -> Char -> Bool
isValidSpace allowTabs c =
    if allowTabs
        then isSpace c
        else isSpace c && c /= '\t'

validSpaceParser
    :: (Maybe String -> (TMS.Token String -> Bool) -> Parser (Tokens String))
    -> (Bool -> Char -> Bool)
    -> Parser ()
validSpaceParser taker validator = do
    tabErrorLevel <- asks psTabErrorLevel
    void $
        tryOrReport
            tabErrorLevel
            "use spaces instead of tabs"
            isUnexpectedTabError
            (taker (Just "valid whitespace") (validator False))
            (taker (Just "valid whitespace") (validator True))

isUnexpectedTabError :: ParseError String Void -> Bool
isUnexpectedTabError (TrivialError _ ue l) =
    ue == Just (Tokens ('\t' :| ""))
        && l == Set.singleton (Label ('v' :| "alid whitespace"))
isUnexpectedTabError _ = False

someValidHSpace :: Parser ()
someValidHSpace = validSpaceParser takeWhile1P isValidHSpace

someValidSpace :: Parser ()
someValidSpace = validSpaceParser takeWhile1P isValidSpace

validHSpace :: Parser ()
validHSpace = validSpaceParser takeWhileP isValidHSpace

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        someValidHSpace
        skipComment
        empty

spaceConsumerN :: Parser ()
spaceConsumerN =
    L.space
        someValidSpace
        skipComment
        empty

-- This catch-all character class is used in a variety of places, and includes characters
-- which have syntactic function. As we continue to iterate on the parser, we may want to consider
-- shrinking or eliminating `contentChar`.
contentChar :: Parser Char
contentChar =
    choice
        [ alphaNumChar
        , char '.'
        , char '['
        , char ']'
        , char '_'
        , char '\''
        , char '"'
        , char '!'
        , char '~'
        , char '-'
        , char ':'
        , char ','
        , do
            backslash <- char '\\'
            nextChar <- lookAhead anySingle
            if nextChar == '(' || nextChar == ')'
                then single nextChar
                else pure backslash
        ]

nonLineSpaceChar :: Parser Char
nonLineSpaceChar = choice [char ' ', char '\t']

-- This is a replacement for `Text.Megaparsec.Char.Lexer.charLiteral`;
-- it does nearly the same thing but additionally supports escaped parentheses.
charLiteral :: Parser Char
charLiteral = label "literal character" $ do
    char1 <- anySingle
    case char1 of
        '\\' -> do
            char2 <- anySingle
            case char2 of
                '(' -> pure '('
                ')' -> pure ')'
                '\\' -> pure '\\'
                '\"' -> pure '\"'
                '\'' -> pure '\''
                _ -> unexpected (Tokens $ char2 :| [])
        _ -> pure char1

assignment :: Parser Attribute
assignment = label "assignment expression" $ do
    L.lexeme spaceConsumer $ do
        lhs <- some contentChar
        _ <- char '='
        rhs <-
            choice
                [ quotation
                , sqlLiteral
                , parentheticalInner
                , try sqlFunctionApplication
                , some $ contentChar <|> char '(' <|> char ')'
                ]
        pure $ Assignment (Text.pack lhs) (Text.pack rhs)
  where
    parentheticalInner = do
        str <- parenthetical'
        pure . init . drop 1 $ str
    sqlFunctionApplication = do
        fn <- some contentChar
        argString <- parentheticalInner
        pure $ mconcat [fn, "(", argString, ")"]

sqlTypeName :: Parser String
sqlTypeName =
    some $
        choice
            [ alphaNumChar
            , char '_'
            ]

sqlLiteral :: Parser String
sqlLiteral = label "SQL literal" $ do
    quote <- L.lexeme spaceConsumer $ char '\'' *> manyTill charLiteral (char '\'')
    st <- optional $ do
        colons <- string "::"
        tn <- sqlTypeName
        pure $ colons <> tn
    pure $
        mconcat
            [ "'"
            , quote
            , "'"
            , fromMaybe "" st
            ]

quotation :: Parser String
quotation = char '"' *> manyTill charLiteral (char '"')

parenthetical :: Parser Attribute
parenthetical = label "parenthetical" $ do
    str <- L.lexeme spaceConsumer parenthetical'
    pure . Parenthetical . Text.pack . init . drop 1 $ str

parenthetical' :: Parser String
parenthetical' = do
    str <- between (char '(') (char ')') q
    pure $ "(" ++ str ++ ")"
  where
    q = mconcat <$> some (c <|> parenthetical')
    c = (: []) <$> choice [contentChar, nonLineSpaceChar, char '"']

blockKey :: Parser BlockKey
blockKey = label "block key" $ do
    fl <- upperChar
    rl <- many alphaNumChar
    pure . BlockKey . Text.pack $ fl : rl

fieldStrictness :: Parser FieldStrictness
fieldStrictness =
    label "strictness sigil" $
        (Strict <$ char '!') <|> (Lazy <$ char '~')

fieldName :: Parser FieldName
fieldName = label "field name" $ do
    fl <- lowerChar
    rl <- many fieldNameChar
    pure . FieldName . Text.pack $ fl : rl
  where
    fieldNameChar =
        choice
            [ alphaNumChar
            , char '_'
            ]

ptext :: Parser Attribute
ptext = label "plain attribute" $ do
    str <- L.lexeme spaceConsumer $ some contentChar
    pure . PText . Text.pack $ str

data ParsedEntityDef = ParsedEntityDef
    { parsedEntityDefComments :: [Text]
    , parsedEntityDefEntityName :: EntityNameHS
    , parsedEntityDefIsSum :: Bool
    , parsedEntityDefEntityAttributes :: [Attribute]
    , parsedEntityDefFields :: [(EntityField, Maybe Text)]
    , parsedEntityDefDirectives :: [(Directive, Maybe Text)]
    , parsedEntityDefExtras :: M.Map Text [ExtraLine]
    , parsedEntityDefSpan :: Maybe SourceSpan
    }
    deriving (Show)

data DocCommentBlock = DocCommentBlock
    { docCommentBlockLines :: [Text]
    , docCommentBlockPos :: SourcePos
    }
    deriving (Show)

data EntityHeader = EntityHeader
    { entityHeaderSum :: Bool
    , entityHeaderTableName :: Text
    , entityHeaderRemainingAttributes :: [Attribute]
    , entityHeaderPos :: SourcePos
    }
    deriving (Show)

data EntityBlock = EntityBlock
    { entityBlockDocCommentBlock :: Maybe DocCommentBlock
    , entityBlockEntityHeader :: EntityHeader
    , entityBlockMembers :: [Member]
    }
    deriving (Show)

entityBlockFirstPos :: EntityBlock -> SourcePos
entityBlockFirstPos = entityHeaderPos . entityBlockEntityHeader

entityBlockLastPos :: EntityBlock -> SourcePos
entityBlockLastPos eb = case entityBlockMembers eb of
    [] -> entityBlockFirstPos eb
    members -> maximum $ fmap memberEndPos members

entityBlockEntityFields :: EntityBlock -> [EntityField]
entityBlockEntityFields = foldMap f <$> entityBlockMembers
  where
    f m = case m of
        MemberExtraBlock _ -> []
        MemberEntityField ba -> [ba]
        MemberDirective _ -> []

entityBlockExtraBlocks :: EntityBlock -> [ExtraBlock]
entityBlockExtraBlocks = foldMap f <$> entityBlockMembers
  where
    f m = case m of
        MemberExtraBlock eb -> [eb]
        MemberEntityField _ -> []
        MemberDirective _ -> []

entityBlockDirectives :: EntityBlock -> [Directive]
entityBlockDirectives = foldMap f <$> entityBlockMembers
  where
    f m = case m of
        MemberExtraBlock _ -> []
        MemberEntityField _ -> []
        MemberDirective bd -> [bd]

data ExtraBlockHeader = ExtraBlockHeader
    { extraBlockHeaderKey :: Text
    , extraBlockHeaderRemainingAttributes :: [Attribute]
    , extraBlockHeaderPos :: SourcePos
    }
    deriving (Show)

data ExtraBlock = ExtraBlock
    { extraBlockDocCommentBlock :: Maybe DocCommentBlock
    , extraBlockExtraBlockHeader :: ExtraBlockHeader
    , extraBlockLines :: NonEmpty ExtraBlockLine
    }
    deriving (Show)

data FieldStrictness = Strict | Lazy
    deriving (Show)

newtype FieldName = FieldName Text
    deriving (Show, Eq)

newtype DirectiveName = DirectiveName Text
    deriving (Show, Eq)

data EntityField = EntityField
    { entityFieldDocCommentBlock :: Maybe DocCommentBlock
    , entityFieldStrictness :: Maybe FieldStrictness
    , entityFieldName :: FieldName
    , entityFieldType :: TypeExpr
    , entityFieldAttributes :: [Attribute]
    , entityFieldPos :: SourcePos
    }
    deriving (Show)

data Directive = Directive
    { directiveDocCommentBlock :: Maybe DocCommentBlock
    , directiveName :: DirectiveName
    , directiveAttributes :: [Attribute]
    , directivePos :: SourcePos
    }
    deriving (Show)

data Member
    = MemberExtraBlock ExtraBlock
    | MemberEntityField EntityField
    | MemberDirective Directive
    deriving (Show)

data ExtraBlockLine = ExtraBlockLine
    { extraBlockLineDocCommentBlock :: Maybe DocCommentBlock
    , extraBlockLineTokens :: [String]
    , extraBlockLinePos :: SourcePos
    }
    deriving (Show)

-- | The source position at the beginning of the member's final line.
memberEndPos :: Member -> SourcePos
memberEndPos (MemberEntityField fs) = entityFieldPos fs
memberEndPos (MemberDirective d) = directivePos d
memberEndPos (MemberExtraBlock ex) = extraBlockLinePos . NEL.last . extraBlockLines $ ex

-- | Represents an entity member as a list of EntityFields
--
-- @since 2.16.0.0
memberEntityFields :: Member -> [EntityField]
memberEntityFields (MemberEntityField fs) = [fs]
memberEntityFields (MemberDirective _) = []
memberEntityFields (MemberExtraBlock _) = []

extraBlocksAsMap :: [ExtraBlock] -> M.Map Text [ExtraLine]
extraBlocksAsMap exs = M.fromList $ fmap asPair exs
  where
    asPair ex =
        ( extraBlockHeaderKey . extraBlockExtraBlockHeader $ ex
        , NEL.toList (extraLines ex)
        )
    extraLines :: ExtraBlock -> NonEmpty [Text]
    extraLines ex = fmap Text.pack . extraBlockLineTokens <$> extraBlockLines ex

entityHeader :: Parser EntityHeader
entityHeader = do
    pos <- getSourcePos
    plus <- optional (char '+')
    en <- validHSpace *> L.lexeme spaceConsumer blockKey
    rest <- L.lexeme spaceConsumer (many attribute)
    _ <- setLastDocumentablePosition
    pure
        EntityHeader
            { entityHeaderSum = isJust plus
            , entityHeaderTableName = blockKeyContent en
            , entityHeaderRemainingAttributes = rest
            , entityHeaderPos = pos
            }

appendCommentToState :: (SourcePos, CommentToken) -> Parser ()
appendCommentToState ptok =
    modify $ \es ->
        let
            comments = esPositionedCommentTokens es
         in
            es{esPositionedCommentTokens = ptok : comments}

setLastDocumentablePosition :: Parser ()
setLastDocumentablePosition = do
    pos <- getSourcePos
    modify $ \es -> es{esLastDocumentablePosition = Just pos}

getDcb :: Parser (Maybe DocCommentBlock)
getDcb = do
    es <- get
    let
        comments = reverse $ esPositionedCommentTokens es
    _ <- put es{esPositionedCommentTokens = []}
    let
        candidates = dropWhile (\(_sp, ct) -> not (isDocComment ct)) comments
        filteredCandidates = dropWhile (commentIsIncorrectlyPositioned es) candidates
    pure $ docCommentBlockFromPositionedAttributes filteredCandidates
  where
    commentIsIncorrectlyPositioned
        :: ExtraState -> (SourcePos, CommentToken) -> Bool
    commentIsIncorrectlyPositioned es ptok = case esLastDocumentablePosition es of
        Nothing -> False
        Just lastDocumentablePos -> (sourceLine . fst) ptok <= sourceLine lastDocumentablePos

extraBlock :: Parser Member
extraBlock = L.indentBlock spaceConsumerN innerParser
  where
    mkExtraBlockMember dcb (header, extraBlockLines) =
        MemberExtraBlock
            ExtraBlock
                { extraBlockExtraBlockHeader = header
                , extraBlockLines = ensureNonEmpty extraBlockLines
                , extraBlockDocCommentBlock = dcb
                }
    ensureNonEmpty lines = case NEL.nonEmpty lines of
        Just nel -> nel
        Nothing -> error "unreachable" -- lines is known to be non-empty
    innerParser = do
        dcb <- getDcb
        header <- extraBlockHeader
        pure $
            L.IndentSome
                Nothing
                (return . mkExtraBlockMember dcb . (header,))
                extraBlockLine

extraBlockHeader :: Parser ExtraBlockHeader
extraBlockHeader = do
    pos <- getSourcePos
    tn <- L.lexeme spaceConsumer blockKey
    rest <- L.lexeme spaceConsumer (many attribute)
    _ <- setLastDocumentablePosition
    pure $
        ExtraBlockHeader
            { extraBlockHeaderKey = blockKeyContent tn
            , extraBlockHeaderRemainingAttributes = rest
            , extraBlockHeaderPos = pos
            }

extraBlockLine :: Parser ExtraBlockLine
extraBlockLine = do
    dcb <- getDcb
    pos <- getSourcePos
    tokens <- some $ L.lexeme spaceConsumer (some contentChar)
    _ <- setLastDocumentablePosition
    pure $
        ExtraBlockLine
            { extraBlockLineDocCommentBlock = dcb
            , extraBlockLineTokens = tokens
            , extraBlockLinePos = pos
            }

entityField :: Parser Member
entityField = do
    dcb <- getDcb
    pos <- getSourcePos
    ss <- optional fieldStrictness
    fn <- L.lexeme spaceConsumer fieldName
    ft <- L.lexeme spaceConsumer typeExpr -- Note that `typeExpr` consumes outer parentheses.
    fa <- L.lexeme spaceConsumer (many attribute)
    _ <- setLastDocumentablePosition
    lookAhead (void newline <|> eof)
    pure $
        MemberEntityField
            EntityField
                { entityFieldDocCommentBlock = dcb
                , entityFieldStrictness = ss
                , entityFieldName = fn
                , entityFieldType = ft
                , entityFieldAttributes = fa
                , entityFieldPos = pos
                }

directiveNameP :: Parser DirectiveName
directiveNameP =
    label "directive name" $
        DirectiveName . Text.pack
            <$> choice
                [ string "deriving"
                , directiveName'
                ]
  where
    directiveName' = do
        fl <- upperChar
        rl <- many alphaNumChar
        pure (fl : rl)

directive :: Parser Member
directive = do
    dcb <- getDcb
    pos <- getSourcePos
    dn <- L.lexeme spaceConsumer directiveNameP
    args <- many $ L.lexeme spaceConsumer attribute
    _ <- setLastDocumentablePosition
    lookAhead (void newline <|> eof)
    pure $
        MemberDirective
            Directive
                { directiveDocCommentBlock = dcb
                , directiveName = dn
                , directiveAttributes = args
                , directivePos = pos
                }

member :: Parser Member
member =
    choice
        [ try extraBlock
        , directive
        , entityField
        ]

entityBlock :: Parser EntityBlock
entityBlock = do
    L.indentBlock spaceConsumerN innerParser
  where
    mkEntityBlock dcb (header, members) =
        EntityBlock
            { entityBlockEntityHeader = header
            , entityBlockMembers = members
            , entityBlockDocCommentBlock = dcb
            }
    innerParser = do
        dcb <- getDcb
        header <- entityHeader
        pure $ L.IndentMany Nothing (return . mkEntityBlock dcb . (header,)) member

entitiesFromDocument :: Parser [EntityBlock]
entitiesFromDocument = many entityBlock

docCommentBlockText :: DocCommentBlock -> Text
docCommentBlockText dcb = Text.unlines $ docCommentBlockLines dcb

isDocComment :: CommentToken -> Bool
isDocComment tok = case tok of
    DocComment _ -> True
    _ -> False

docCommentBlockFromPositionedAttributes
    :: [(SourcePos, CommentToken)] -> Maybe DocCommentBlock
docCommentBlockFromPositionedAttributes ptoks =
    case NEL.nonEmpty ptoks of
        Nothing -> Nothing
        Just nel ->
            Just $
                DocCommentBlock
                    { docCommentBlockLines = NEL.toList $ fmap (commentContent . snd) nel
                    , docCommentBlockPos = fst $ NEL.head nel
                    }

parseEntities
    :: PersistSettings
    -> Text
    -> String
    -> ParseResult [EntityBlock]
parseEntities ps fp s = do
    let
        (warnings, res) =
            runConfiguredParser ps initialExtraState entitiesFromDocument (Text.unpack fp) s
    case res of
        Left peb ->
            (warnings, Left peb)
        Right (entities, _comments) ->
            (warnings, pure entities)

toParsedEntityDef :: Maybe SourceLoc -> EntityBlock -> ParsedEntityDef
toParsedEntityDef mSourceLoc eb =
    ParsedEntityDef
        { parsedEntityDefComments = comments
        , parsedEntityDefEntityName = entityNameHS
        , parsedEntityDefIsSum = isSum
        , parsedEntityDefEntityAttributes = entityAttributes
        , parsedEntityDefFields = parsedFields
        , parsedEntityDefDirectives = parsedDirectives
        , parsedEntityDefExtras = extras
        , parsedEntityDefSpan = mSpan
        }
  where
    comments =
        maybe
            []
            docCommentBlockLines
            (entityBlockDocCommentBlock eb)
    entityAttributes = entityHeaderRemainingAttributes . entityBlockEntityHeader $ eb
    isSum = entityHeaderSum . entityBlockEntityHeader $ eb
    entityNameHS = EntityNameHS . entityHeaderTableName . entityBlockEntityHeader $ eb

    fieldPair a = (a, docCommentBlockText <$> entityFieldDocCommentBlock a)
    parsedFields = fmap fieldPair (entityBlockEntityFields eb)

    directivePair d = (d, docCommentBlockText <$> directiveDocCommentBlock d)
    parsedDirectives = fmap directivePair (entityBlockDirectives eb)

    extras = extraBlocksAsMap (entityBlockExtraBlocks eb)
    filepath = maybe "" locFile mSourceLoc
    relativeStartLine = maybe 0 locStartLine mSourceLoc
    relativeStartCol = maybe 0 locStartCol mSourceLoc
    mSpan =
        Just
            SourceSpan
                { spanFile = filepath
                , spanStartLine =
                    relativeStartLine + (unPos . sourceLine $ entityBlockFirstPos eb)
                , spanEndLine = relativeStartLine + (unPos . sourceLine $ entityBlockLastPos eb)
                , spanStartCol =
                    relativeStartCol + (unPos . sourceColumn $ entityBlockFirstPos eb)
                , spanEndCol = unPos . sourceColumn $ entityBlockLastPos eb
                }

parseSource
    :: PersistSettings
    -> Maybe SourceLoc
    -> Text
    -> ParseResult [ParsedEntityDef]
parseSource ps mSourceLoc source =
    fmap (fmap (toParsedEntityDef mSourceLoc))
        <$> parseEntities ps filepath (Text.unpack source)
  where
    filepath = maybe "" locFile mSourceLoc
