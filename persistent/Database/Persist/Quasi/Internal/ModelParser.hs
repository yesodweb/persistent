{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Database.Persist.Quasi.Internal.ModelParser
    ( SourceLoc (..)
    , Token (..)
    , tokenContent
    , anyToken
    , ParsedEntityDef
    , parsedEntityDefComments
    , parsedEntityDefEntityName
    , parsedEntityDefIsSum
    , parsedEntityDefEntityAttributes
    , parsedEntityDefFieldAttributes
    , parsedEntityDefExtras
    , parsedEntityDefSpan
    , parseSource
    , memberBlockAttrs
    , ParseResult
    , CumulativeParseResult
    , toCumulativeParseResult
    , renderErrors
    , runConfiguredParser
    , initialExtraState
    ) where

import Control.Monad (void)
import Control.Monad.Trans.State
import Data.Either (partitionEithers)
import Data.Foldable (fold)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void
import Database.Persist.Types
import Database.Persist.Types.SourceSpan
import Language.Haskell.TH.Syntax (Lift)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- We'll augment the parser with extra state to accumulate comments seen during parsing.
-- Comments are lexed as whitespace, but will be used to generate documentation later.
data ExtraState = ExtraState
    { esPositionedCommentTokens :: [(SourcePos, CommentToken)]
    , esLastDocumentablePosition :: Maybe SourcePos
    }

-- @since 2.16.1.0
initialExtraState :: ExtraState
initialExtraState =
    ExtraState
        { esPositionedCommentTokens = []
        , esLastDocumentablePosition = Nothing
        }

type Parser a =
    StateT
        ExtraState
        (Parsec Void String)
        a

type EntityParseError = ParseErrorBundle String Void
type InternalParseResult a = Either EntityParseError (a, ExtraState)

type ParseResult a = Either EntityParseError a
type CumulativeParseResult a = Either [EntityParseError] a

-- | Run a parser using a provided ExtraState
-- @since 2.16.0.0
runConfiguredParser
    :: ExtraState
    -> Parser a
    -> String
    -> String
    -> InternalParseResult a
runConfiguredParser acc parser fp s = parseResult
  where
    (_internalState, parseResult) = runParser' (runStateT parser acc) initialInternalState
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

-- | Renders a list of EntityParseErrors as a String using `errorBundlePretty`,
-- separated by line breaks.
-- @since 2.16.0.0
renderErrors :: [EntityParseError] -> String
renderErrors errs = intercalate "\n" $ fmap errorBundlePretty errs

toCumulativeParseResult
    :: (Monoid a) => [ParseResult a] -> CumulativeParseResult a
toCumulativeParseResult prs = case partitionEithers prs of
    ([], results) -> Right $ fold results
    (errs, _) -> Left errs

-- | Source location: file and line/col information. This is half of a 'SourceSpan'.
--
-- @since 2.16.0.0
data SourceLoc = SourceLoc
    { locFile :: Text
    , locStartLine :: Int
    , locStartCol :: Int
    }
    deriving (Show, Lift)

-- @since 2.16.0.0
data Token
    = Quotation Text
    | Equality Text Text
    | Parenthetical Text
    | BlockKey Text
    | PText Text
    deriving (Eq, Ord, Show)

-- @since 2.16.0.0
data CommentToken
    = DocComment Text
    | Comment Text
    deriving (Eq, Ord, Show)

-- | Converts a token into a Text representation for second-stage parsing or presentation to the user
--
-- @since 2.16.0.0
tokenContent :: Token -> Text
tokenContent = \case
    Quotation s -> s
    Equality l r -> mconcat [l, "=", r]
    Parenthetical s -> s
    PText s -> s
    BlockKey s -> s

commentContent :: CommentToken -> Text
commentContent = \case
    Comment s -> s
    DocComment s -> s

docComment :: Parser (SourcePos, CommentToken)
docComment = do
    pos <- getSourcePos
    content <- string "-- |" *> hspace *> takeWhileP (Just "character") (/= '\n')
    pure (pos, DocComment (Text.pack content))

comment :: Parser (SourcePos, CommentToken)
comment = do
    pos <- getSourcePos
    content <-
        (string "--" <|> string "#")
            *> hspace
            *> takeWhileP (Just "character") (/= '\n')
    pure (pos, Comment (Text.pack content))

skipComment :: Parser ()
skipComment = do
    content <- docComment <|> comment
    void $ appendCommentToState content

spaceConsumer :: Parser ()
spaceConsumer =
    L.space
        hspace1
        skipComment
        empty

spaceConsumerN :: Parser ()
spaceConsumerN =
    L.space
        space1
        skipComment
        empty

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

equality :: Parser Token
equality = label "equality expression" $ do
    L.lexeme spaceConsumer $ do
        lhs <- some contentChar
        _ <- char '='
        rhs <-
            choice
                [ quotation'
                , sqlLiteral
                , parentheticalInner
                , some $ contentChar <|> char '(' <|> char ')'
                ]
        pure $ Equality (Text.pack lhs) (Text.pack rhs)
  where
    parentheticalInner = do
        str <- parenthetical'
        pure . init . drop 1 $ str

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

quotation :: Parser Token
quotation = label "quotation" $ do
    str <- L.lexeme spaceConsumer quotation'
    pure . Quotation $ Text.pack str

quotation' :: Parser String
quotation' = char '"' *> manyTill charLiteral (char '"')

parenthetical :: Parser Token
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

blockKey :: Parser Token
blockKey = label "block key" $ do
    fl <- upperChar
    rl <- many alphaNumChar
    pure . BlockKey . Text.pack $ fl : rl

ptext :: Parser Token
ptext = label "plain token" $ do
    str <- L.lexeme spaceConsumer $ some contentChar
    pure . PText . Text.pack $ str

-- @since 2.16.0.0
anyToken :: Parser Token
anyToken =
    choice
        [ try equality
        , quotation
        , parenthetical
        , ptext
        ]

data ParsedEntityDef = ParsedEntityDef
    { parsedEntityDefComments :: [Text]
    , parsedEntityDefEntityName :: EntityNameHS
    , parsedEntityDefIsSum :: Bool
    , parsedEntityDefEntityAttributes :: [Attr]
    , parsedEntityDefFieldAttributes :: [([Token], Maybe Text)]
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
    , entityHeaderRemainingTokens :: [Token]
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

entityBlockBlockAttrs :: EntityBlock -> [BlockAttr]
entityBlockBlockAttrs = foldMap f <$> entityBlockMembers
  where
    f m = case m of
        MemberExtraBlock _ -> []
        MemberBlockAttr ba -> [ba]

entityBlockExtraBlocks :: EntityBlock -> [ExtraBlock]
entityBlockExtraBlocks = foldMap f <$> entityBlockMembers
  where
    f m = case m of
        MemberExtraBlock eb -> [eb]
        MemberBlockAttr _ -> []

data ExtraBlockHeader = ExtraBlockHeader
    { extraBlockHeaderKey :: Text
    , extraBlockHeaderRemainingTokens :: [Token]
    , extraBlockHeaderPos :: SourcePos
    }
    deriving (Show)

data ExtraBlock = ExtraBlock
    { extraBlockDocCommentBlock :: Maybe DocCommentBlock
    , extraBlockExtraBlockHeader :: ExtraBlockHeader
    , extraBlockMembers :: NonEmpty Member
    }
    deriving (Show)

data BlockAttr = BlockAttr
    { blockAttrDocCommentBlock :: Maybe DocCommentBlock
    , blockAttrTokens :: [Token]
    , blockAttrPos :: SourcePos
    }
    deriving (Show)

data Member = MemberExtraBlock ExtraBlock | MemberBlockAttr BlockAttr
    deriving (Show)

-- | The source position at the beginning of the member's final line.
memberEndPos :: Member -> SourcePos
memberEndPos (MemberBlockAttr fs) = blockAttrPos fs
memberEndPos (MemberExtraBlock ex) = memberEndPos . NEL.last . extraBlockMembers $ ex

-- | Represents an entity member as a list of BlockAttrs
--
-- @since 2.16.0.0
memberBlockAttrs :: Member -> [BlockAttr]
memberBlockAttrs (MemberBlockAttr fs) = [fs]
memberBlockAttrs (MemberExtraBlock ex) = foldMap memberBlockAttrs . extraBlockMembers $ ex

extraBlocksAsMap :: [ExtraBlock] -> M.Map Text [ExtraLine]
extraBlocksAsMap exs = M.fromList $ fmap asPair exs
  where
    asPair ex =
        (extraBlockHeaderKey . extraBlockExtraBlockHeader $ ex, extraLines ex)
    extraLines ex = foldMap asExtraLine (extraBlockMembers ex)
    asExtraLine (MemberBlockAttr fs) = [tokenContent <$> blockAttrTokens fs]
    asExtraLine _ = []

entityHeader :: Parser EntityHeader
entityHeader = do
    pos <- getSourcePos
    plus <- optional (char '+')
    en <- hspace *> L.lexeme spaceConsumer blockKey
    rest <- L.lexeme spaceConsumer (many anyToken)
    _ <- setLastDocumentablePosition
    pure
        EntityHeader
            { entityHeaderSum = isJust plus
            , entityHeaderTableName = tokenContent en
            , entityHeaderRemainingTokens = rest
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
    pure $ docCommentBlockFromPositionedTokens filteredCandidates
  where
    commentIsIncorrectlyPositioned
        :: ExtraState -> (SourcePos, CommentToken) -> Bool
    commentIsIncorrectlyPositioned es ptok = case esLastDocumentablePosition es of
        Nothing -> False
        Just lastDocumentablePos -> (sourceLine . fst) ptok <= sourceLine lastDocumentablePos

extraBlock :: Parser Member
extraBlock = L.indentBlock spaceConsumerN innerParser
  where
    mkExtraBlockMember dcb (header, blockAttrs) =
        MemberExtraBlock
            ExtraBlock
                { extraBlockExtraBlockHeader = header
                , extraBlockMembers = ensureNonEmpty blockAttrs
                , extraBlockDocCommentBlock = dcb
                }
    ensureNonEmpty members = case NEL.nonEmpty members of
        Just nel -> nel
        Nothing -> error "unreachable" -- members is known to be non-empty
    innerParser = do
        dcb <- getDcb
        header <- extraBlockHeader
        pure $
            L.IndentSome Nothing (return . mkExtraBlockMember dcb . (header,)) blockAttr

extraBlockHeader :: Parser ExtraBlockHeader
extraBlockHeader = do
    pos <- getSourcePos
    tn <- L.lexeme spaceConsumer blockKey
    rest <- L.lexeme spaceConsumer (many anyToken)
    _ <- setLastDocumentablePosition
    pure $
        ExtraBlockHeader
            { extraBlockHeaderKey = tokenContent tn
            , extraBlockHeaderRemainingTokens = rest
            , extraBlockHeaderPos = pos
            }

blockAttr :: Parser Member
blockAttr = do
    dcb <- getDcb
    pos <- getSourcePos
    line <- some anyToken
    _ <- setLastDocumentablePosition
    pure $
        MemberBlockAttr
            BlockAttr
                { blockAttrDocCommentBlock = dcb
                , blockAttrTokens = line
                , blockAttrPos = pos
                }

member :: Parser Member
member = try extraBlock <|> blockAttr

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

docCommentBlockFromPositionedTokens
    :: [(SourcePos, CommentToken)] -> Maybe DocCommentBlock
docCommentBlockFromPositionedTokens ptoks =
    case NEL.nonEmpty ptoks of
        Nothing -> Nothing
        Just nel ->
            Just $
                DocCommentBlock
                    { docCommentBlockLines = NEL.toList $ fmap (commentContent . snd) nel
                    , docCommentBlockPos = fst $ NEL.head nel
                    }

parseEntities
    :: Text
    -> String
    -> ParseResult [EntityBlock]
parseEntities fp s = do
    (entities, _comments) <-
        runConfiguredParser initialExtraState entitiesFromDocument (Text.unpack fp) s
    pure entities

toParsedEntityDef :: Maybe SourceLoc -> EntityBlock -> ParsedEntityDef
toParsedEntityDef mSourceLoc eb =
    ParsedEntityDef
        { parsedEntityDefComments = comments
        , parsedEntityDefEntityName = entityNameHS
        , parsedEntityDefIsSum = isSum
        , parsedEntityDefEntityAttributes = entityAttributes
        , parsedEntityDefFieldAttributes = parsedFieldAttributes
        , parsedEntityDefExtras = extras
        , parsedEntityDefSpan = mSpan
        }
  where
    comments =
        maybe
            []
            docCommentBlockLines
            (entityBlockDocCommentBlock eb)
    entityAttributes =
        tokenContent <$> (entityHeaderRemainingTokens . entityBlockEntityHeader) eb
    isSum = entityHeaderSum . entityBlockEntityHeader $ eb
    entityNameHS = EntityNameHS . entityHeaderTableName . entityBlockEntityHeader $ eb

    attributePair a = (blockAttrTokens a, docCommentBlockText <$> blockAttrDocCommentBlock a)
    parsedFieldAttributes = fmap attributePair (entityBlockBlockAttrs eb)

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

parseSource :: Maybe SourceLoc -> Text -> ParseResult [ParsedEntityDef]
parseSource mSourceLoc source =
    case parseEntities filepath (Text.unpack source) of
        Right blocks -> Right (toParsedEntityDef mSourceLoc <$> blocks)
        Left peb -> Left peb
  where
    filepath = maybe "" locFile mSourceLoc
