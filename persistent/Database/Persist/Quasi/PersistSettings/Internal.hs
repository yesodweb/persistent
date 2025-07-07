module Database.Persist.Quasi.PersistSettings.Internal where

import Data.Char (isDigit, isLower, isSpace, isUpper, toLower)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Database.Persist.Names
import Database.Persist.Types
import Text.Megaparsec
    ( ParseError
    , ParseErrorBundle (..)
    , PosState
    , SourcePos
    , errorBundlePretty
    , pstateSourcePos
    )

data PersistSettings = PersistSettings
    { psToDBName :: !(Text -> Text)
    -- ^ Modify the Haskell-style name into a database-style name.
    , psToFKName :: !(EntityNameHS -> ConstraintNameHS -> Text)
    -- ^ A function for generating the constraint name, with access to
    -- the entity and constraint names. Default value: @mappend@
    --
    -- @since 2.13.0.0
    , psStrictFields :: !Bool
    -- ^ Whether fields are by default strict. Default value: @True@.
    --
    -- @since 1.2
    , psIdName :: !Text
    -- ^ The name of the id column. Default value: @id@
    -- The name of the id column can also be changed on a per-model basis
    -- <https://github.com/yesodweb/persistent/wiki/Persistent-entity-syntax>
    --
    -- @since 2.0
    , psTabErrorLevel :: Maybe ParserErrorLevel
    -- ^ Whether and with what severity to disallow tabs in entity source text.
    --
    -- @since 2.16.0.0
    , psQuotedArgumentErrorLevel :: Maybe ParserErrorLevel
    -- ^ Whether and with what severity to disallow quoted entity field attributes
    -- and quoted directive arguments.
    --
    -- @since 2.17.1.0
    }

defaultPersistSettings, upperCaseSettings, lowerCaseSettings :: PersistSettings
defaultPersistSettings =
    PersistSettings
        { psToDBName = id
        , psToFKName = \(EntityNameHS entName) (ConstraintNameHS conName) -> entName <> conName
        , psStrictFields = True
        , psIdName = "id"
        , psTabErrorLevel = Just LevelWarning
        , psQuotedArgumentErrorLevel = Just LevelWarning
        }
upperCaseSettings = defaultPersistSettings
lowerCaseSettings =
    defaultPersistSettings
        { psToDBName =
            let
                go c
                    | isUpper c = T.pack ['_', toLower c]
                    | otherwise = T.singleton c
             in
                T.dropWhile (== '_') . T.concatMap go
        }

-- |
--
-- @since 2.16.0.0
data ParserErrorLevel = LevelError | LevelWarning deriving (Eq, Show)

-- |
--
-- @since 2.16.0.0
data ParserWarning = ParserWarning
    { parserWarningExtraMessage :: String
    , parserWarningUnderlyingError :: ParseError String Void
    , parserWarningPosState :: PosState String
    }
    deriving (Eq, Show)

warningPos :: ParserWarning -> SourcePos
warningPos = pstateSourcePos . parserWarningPosState

instance Ord ParserWarning where
    l <= r =
        if warningPos l == warningPos r
            then parserWarningMessage l <= parserWarningMessage r
            else warningPos l <= warningPos r

-- | Uses @errorBundlePretty@ to render a parser warning.
--
-- @since 2.16.0.0
parserWarningMessage :: ParserWarning -> String
parserWarningMessage pw =
    parserWarningExtraMessage pw
        <> ( errorBundlePretty $
                ParseErrorBundle
                    { bundleErrors = parserWarningUnderlyingError pw :| []
                    , bundlePosState = parserWarningPosState pw
                    }
           )

toFKNameInfixed :: Text -> EntityNameHS -> ConstraintNameHS -> Text
toFKNameInfixed inf (EntityNameHS entName) (ConstraintNameHS conName) =
    entName <> inf <> conName

-- | Retrieve the function in the 'PersistSettings' that modifies the names into
-- database names.
--
-- @since 2.13.0.0
getPsToDBName :: PersistSettings -> Text -> Text
getPsToDBName = psToDBName

-- | Set the name modification function that translates the QuasiQuoted names
-- for use in the database.
--
-- @since 2.13.0.0
setPsToDBName :: (Text -> Text) -> PersistSettings -> PersistSettings
setPsToDBName f ps = ps{psToDBName = f}

-- | Set a custom function used to create the constraint name
-- for a foreign key.
--
-- @since 2.13.0.0
setPsToFKName
    :: (EntityNameHS -> ConstraintNameHS -> Text) -> PersistSettings -> PersistSettings
setPsToFKName setter ps = ps{psToFKName = setter}

-- | A preset configuration function that puts an underscore
-- between the entity name and the constraint name when
-- creating a foreign key constraint name
--
-- @since 2.14.2.0
setPsUseSnakeCaseForeignKeys :: PersistSettings -> PersistSettings
setPsUseSnakeCaseForeignKeys = setPsToFKName (toFKNameInfixed "_")

-- | Equivalent to 'setPsUseSnakeCaseForeignKeys', but misspelled.
--
-- @since 2.13.0.0
setPsUseSnakeCaseForiegnKeys :: PersistSettings -> PersistSettings
setPsUseSnakeCaseForiegnKeys = setPsUseSnakeCaseForeignKeys
{-# DEPRECATED
    setPsUseSnakeCaseForiegnKeys
    "use the correctly spelled, equivalent, setPsUseSnakeCaseForeignKeys instead"
    #-}

-- | Retrieve whether or not the 'PersistSettings' will generate code with
-- strict fields.
--
-- @since 2.13.0.0
getPsStrictFields :: PersistSettings -> Bool
getPsStrictFields = psStrictFields

-- | Set whether or not the 'PersistSettings' will make fields strict.
--
-- @since 2.13.0.0
setPsStrictFields :: Bool -> PersistSettings -> PersistSettings
setPsStrictFields a ps = ps{psStrictFields = a}

-- | Retrieve the default name of the @id@ column.
--
-- @since 2.13.0.0
getPsIdName :: PersistSettings -> Text
getPsIdName = psIdName

-- | Set the default name of the @id@ column.
--
-- @since 2.13.0.0
setPsIdName :: Text -> PersistSettings -> PersistSettings
setPsIdName n ps = ps{psIdName = n}

-- | Retrieve the severity of the error generated when the parser encounters a tab.
-- If it is @Nothing@, tabs are permitted in entity definitions.
--
-- @since 2.16.0.0
getPsTabErrorLevel :: PersistSettings -> Maybe ParserErrorLevel
getPsTabErrorLevel = psTabErrorLevel

-- | Set the severity of the error generated when the parser encounters a tab.
-- If set to @Nothing@, tabs are permitted in entity definitions.
--
-- @since 2.16.0.0
setPsTabErrorLevel
    :: Maybe ParserErrorLevel -> PersistSettings -> PersistSettings
setPsTabErrorLevel l ps = ps{psTabErrorLevel = l}

-- | Retrieve the severity of the error generated when the parser encounters a
-- quoted entity field attribute or quoted directive argument.
-- If it is @Nothing@, quoted arguments are permitted in both entity field
-- definitions and directives.
--
-- @since 2.17.1.0
getPsQuotedArgumentErrorLevel :: PersistSettings -> Maybe ParserErrorLevel
getPsQuotedArgumentErrorLevel = psQuotedArgumentErrorLevel

-- | Set the severity of the error generated when the parser encounters a
-- quoted entity field attribute.
-- If set to @Nothing@, quoted arguments are permitted in both entity field
-- definitions and directives.
--
-- @since 2.17.1.0
setPsQuotedArgumentErrorLevel
    :: Maybe ParserErrorLevel -> PersistSettings -> PersistSettings
setPsQuotedArgumentErrorLevel l ps = ps{psQuotedArgumentErrorLevel = l}
