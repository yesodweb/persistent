{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Persist.QuasiSpec where

import Prelude hiding (lines)

import Control.Exception
import Data.Bifunctor
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Database.Persist.EntityDef.Internal
import Database.Persist.Quasi
import Database.Persist.Quasi.Internal
import Database.Persist.Quasi.Internal.ModelParser
import Database.Persist.Quasi.Internal.TypeParser
import Database.Persist.Quasi.PersistSettings
import Database.Persist.Quasi.PersistSettings.Internal
    ( psQuotedArgumentErrorLevel
    , psTabErrorLevel
    )
import Database.Persist.Types
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec (errorBundlePretty, some)
import Text.Shakespeare.Text (st)

defs :: T.Text -> [UnboundEntityDef]
defs = defsWithSettings lowerCaseSettings

defsSnake :: T.Text -> [UnboundEntityDef]
defsSnake = defsWithSettings $ setPsUseSnakeCaseForeignKeys lowerCaseSettings

defsWithWarnings
    :: PersistSettings -> T.Text -> (Set ParserWarning, [UnboundEntityDef])
defsWithWarnings ps t = case cpr of
    (warnings, Right res) -> (warnings, res)
    (_warnings, Left errs) -> error $ renderErrors errs
  where
    cpr = parse ps [(Nothing, t)]

defsWithSettings :: PersistSettings -> T.Text -> [UnboundEntityDef]
defsWithSettings ps t = snd $ defsWithWarnings ps t

#if MIN_VERSION_megaparsec(9,5,0)
warningSpecs :: Spec
warningSpecs =
  describe "Quasi" $ do
      describe "psTabErrorLevel parser setting" $ do
          let
              definitions = T.pack "User\n\tId Text\n\tname String"
              (warnings, [user]) =
                defsWithWarnings lowerCaseSettings { psTabErrorLevel = Just LevelWarning
                                                   }
                                 definitions
          it "generates warnings" $ do
            Set.map parserWarningMessage warnings
              `shouldBe` [ "use spaces instead of tabs\n2:1:\n  |\n2 |  Id Text\n  | ^\nunexpected tab\nexpecting valid whitespace\n"
                         , "use spaces instead of tabs\n3:1:\n  |\n3 |  name String\n  | ^\nunexpected tab\nexpecting valid whitespace\n"
                         ]
      describe "psQuotedArgumentErrorLevel parser setting" $ do
          let
              definitions = T.pack "User\n Id \"Text\"\n name String\n deriving \"Eq\""
              (warnings, [user]) =
                defsWithWarnings lowerCaseSettings { psQuotedArgumentErrorLevel = Just LevelWarning
                                                   }
                                 definitions
          it "generates warnings" $ do
            Set.map parserWarningMessage warnings
              `shouldBe` ["Quoted field attributes are deprecated since 2.17.1.0, and will be removed in or after 2.18.0.0\n2:5:\n  |\n2 | Id \"Text\"\n  |     ^\nUnexpected quotation mark in field or directive attribute\n","Quoted field attributes are deprecated since 2.17.1.0, and will be removed in or after 2.18.0.0\n4:11:\n  |\n4 | deriving \"Eq\"\n  |           ^\nUnexpected quotation mark in field or directive attribute\n"]
#else
warningSpecs :: Spec
warningSpecs = pure ()
#endif

spec :: Spec
spec = describe "Quasi" $ do
    describe "takeColsEx" $ do
        let
            subject = takeColsEx upperCaseSettings
        it "fails on a single word" $ do
            subject ["asdf"]
                `shouldBe` Nothing
        it "errors on invalid input" $ do
            evaluate (subject ["name", "int"])
                `shouldErrorWithMessage` "Invalid field type \"int\" PSFail \"int\""
        it "works if it has a name and a type" $ do
            subject ["asdf", "Int"]
                `shouldBe` Just
                    UnboundFieldDef
                        { unboundFieldNameHS = FieldNameHS "asdf"
                        , unboundFieldNameDB = FieldNameDB "asdf"
                        , unboundFieldType = FTTypeCon Nothing "Int"
                        , unboundFieldAttrs = []
                        , unboundFieldStrict = True
                        , unboundFieldCascade = noCascade
                        , unboundFieldComments = Nothing
                        , unboundFieldGenerated = Nothing
                        }
        it "works if it has a name, type, and cascade" $ do
            subject ["asdf", "Int", "OnDeleteCascade", "OnUpdateCascade"]
                `shouldBe` Just
                    UnboundFieldDef
                        { unboundFieldNameHS = FieldNameHS "asdf"
                        , unboundFieldNameDB = FieldNameDB "asdf"
                        , unboundFieldType = FTTypeCon Nothing "Int"
                        , unboundFieldAttrs = []
                        , unboundFieldStrict = True
                        , unboundFieldCascade = FieldCascade (Just Cascade) (Just Cascade)
                        , unboundFieldComments = Nothing
                        , unboundFieldGenerated = Nothing
                        }
        it "never tries to make a refernece" $ do
            subject ["asdf", "UserId", "OnDeleteCascade"]
                `shouldBe` Just
                    UnboundFieldDef
                        { unboundFieldNameHS = FieldNameHS "asdf"
                        , unboundFieldNameDB = FieldNameDB "asdf"
                        , unboundFieldType = FTTypeCon Nothing "UserId"
                        , unboundFieldAttrs = []
                        , unboundFieldStrict = True
                        , unboundFieldCascade = FieldCascade Nothing (Just Cascade)
                        , unboundFieldComments = Nothing
                        , unboundFieldGenerated = Nothing
                        }

    describe "type parsing" $ do
        let
            parseType :: String -> ParseResult TypeExpr
            parseType s = do
                let
                    (warnings, res) =
                        runConfiguredParser defaultPersistSettings initialExtraState innerTypeExpr "" s
                case res of
                    Left peb -> (warnings, Left peb)
                    Right (te, _acc) -> (warnings, Right te)

            isType typeStr expectedTypeExpr = do
                let
                    (_warnings, Right te) = parseType typeStr
                te `shouldBe` expectedTypeExpr
                typeExprContent te `shouldBe` T.pack typeStr

            -- these are some helper functions to make expectations less verbose
            simpleType s = (TypeApplication (TypeConstructorExpr (TypeConstructor s)) [])
            typeApp s ts = (TypeApplication (TypeConstructorExpr (TypeConstructor s)) ts)
            listOf t = (TypeApplication (TypeConstructorExpr ListConstructor) [t])

        it "parses types of kind '*'" $ do
            "String" `isType` simpleType "String"

        it "parses type constructors with dots" $ do
            "ThisIs.AType" `isType` simpleType "ThisIs.AType"

        it "parses higher-kinded types" $ do
            "Maybe String" `isType` typeApp "Maybe" [simpleType "String"]

        it "is greedy when parsing arguments to a type constructor" $ do
            "Map String Int" `isType` typeApp "Map" [simpleType "String", simpleType "Int"]

        it "parses higher-kinded types when parameterized by complex types (1)" $ do
            "Map String (Maybe [Int])"
                `isType` typeApp "Map" [simpleType "String", typeApp "Maybe" [listOf (simpleType "Int")]]

        it "parses higher-kinded types when parameterized by complex types (2)" $ do
            "Map (Maybe Int) [Int]"
                `isType` typeApp "Map" [(typeApp "Maybe" [simpleType "Int"]), listOf (simpleType "Int")]

        it "parses type expressions constructed by a partially parameterized type" $ do
            "(Map String) [Int]"
                `isType` TypeApplication
                    (typeApp "Map" [(simpleType "String")])
                    [listOf (simpleType "Int")]

        it "parses lists of lists" $ do
            "[[Maybe String]]"
                `isType` listOf (listOf (typeApp "Maybe" [simpleType "String"]))

        it "parses list types of complex types" $ do
            "[(Map String) [Int]]"
                `isType` listOf
                    ( TypeApplication
                        (typeApp "Map" [(simpleType "String")])
                        [listOf (simpleType "Int")]
                    )

        it "parses type-level String literals" $ do
            "Labelled \"abcd\"" `isType` typeApp "Labelled" [TypeLitString "abcd"]

        it "parses type-level Int literals" $ do
            "Val 3" `isType` typeApp "Val" [TypeLitInt "3"]

        it "parses promoted type constructors" $ do
            "'Maybe" `isType` TypeLitPromotedConstructor (TypeConstructor "Maybe")

    describe "field name parsing" $ do
        let
            parseFieldName :: String -> ParseResult FieldName
            parseFieldName s = do
                let
                    (warnings, res) = runConfiguredParser defaultPersistSettings initialExtraState fieldName "" s
                case res of
                    Left peb ->
                        (warnings, Left peb)
                    Right (fn, _acc) -> (warnings, Right fn)

        it "parses alphanumeric field names" $
            parseFieldName "asdf100"
                `shouldBe` ([], Right (FieldName "asdf100"))

        it "parses alphanumeric field names with underscores" $
            parseFieldName "asdf_100"
                `shouldBe` ([], Right (FieldName "asdf_100"))

    describe "attribute parsing" $ do
        let
            parseAttributes :: String -> ParseResult [Attribute]
            parseAttributes s = do
                let
                    (warnings, res) =
                        runConfiguredParser
                            defaultPersistSettings
                            initialExtraState
                            (some attribute)
                            ""
                            s
                case res of
                    Left peb ->
                        (warnings, Left peb)
                    Right (tokens, _acc) -> (warnings, Right tokens)

        it "handles normal words" $
            parseAttributes "foo   bar  baz"
                `shouldBe` ( []
                           , Right
                                ( [ PText "foo"
                                  , PText "bar"
                                  , PText "baz"
                                  ]
                                )
                           )

        it "handles bangs" $
            parseAttributes "foo   !bar  baz"
                `shouldBe` ( []
                           , Right
                                ( [ PText "foo"
                                  , PText "!bar"
                                  , PText "baz"
                                  ]
                                )
                           )

        it "handles numbers" $
            parseAttributes "one (Finite 1)"
                `shouldBe` ( []
                           , Right
                                ( [ PText "one"
                                  , Parenthetical "Finite 1"
                                  ]
                                )
                           )

        it "handles quotes" $
            parseAttributes "abc=\"foo bar\" def=\"baz\""
                `shouldBe` ( []
                           , Right
                                ( [ Assignment "abc" "foo bar"
                                  , Assignment "def" "baz"
                                  ]
                                )
                           )

        it "handles SQL literals with no specified type" $
            parseAttributes "attr='[\"ab\\'cd\", 1, 2]'"
                `shouldBe` ( []
                           , Right
                                ([Assignment "attr" "'[\"ab'cd\", 1, 2]'"])
                           )

        it "handles SQL literals with a specified type" $
            parseAttributes "attr='{\"\\'a\\'\": [1, 2.2, \"\\'3\\'\"]}'::type_name"
                `shouldBe` ( []
                           , Right
                                ([Assignment "attr" "'{\"'a'\": [1, 2.2, \"'3'\"]}'::type_name"])
                           )

        it "handles commas in tokens" $
            parseAttributes "x=COALESCE(left,right)  baz"
                `shouldBe` ( []
                           , Right
                                ( [ Assignment "x" "COALESCE(left,right)"
                                  , PText "baz"
                                  ]
                                )
                           )

        it "handles single quotes in tokens" $
            parseAttributes "x=blorp('blap')  baz"
                `shouldBe` ( []
                           , Right
                                ( [ Assignment "x" "blorp('blap')"
                                  , PText "baz"
                                  ]
                                )
                           )

        it "handles spaces in assignment RHSes" $
            parseAttributes "sql=blorp('blap', 'blip')  baz"
                `shouldBe` ( []
                           , Right
                                ( [ Assignment "sql" "blorp('blap', 'blip')"
                                  , PText "baz"
                                  ]
                                )
                           )

        it "handles quotes mid-token" $
            parseAttributes "x=\"foo bar\"  baz"
                `shouldBe` ( []
                           , Right
                                ( [ Assignment "x" "foo bar"
                                  , PText "baz"
                                  ]
                                )
                           )

        it "handles escaped quotes mid-token" $
            parseAttributes "x=\\\"foo bar\"  baz"
                `shouldBe` ( []
                           , Right
                                ( [ Assignment "x" "\\\"foo"
                                  , PText "bar\""
                                  , PText "baz"
                                  ]
                                )
                           )

        it "handles unnested parentheses" $
            parseAttributes "(foo bar)  (baz)"
                `shouldBe` ( []
                           , Right
                                ( [ Parenthetical "foo bar"
                                  , Parenthetical "baz"
                                  ]
                                )
                           )

        it "handles unnested parentheses mid-token" $
            parseAttributes "x=(foo bar)  (baz)"
                `shouldBe` ( []
                           , Right
                                ( [ Assignment "x" "foo bar"
                                  , Parenthetical "baz"
                                  ]
                                )
                           )

        it "handles nested parentheses" $
            parseAttributes "(foo (bar))  (baz)"
                `shouldBe` ( []
                           , Right
                                ( [ Parenthetical "foo (bar)"
                                  , Parenthetical "baz"
                                  ]
                                )
                           )

        it "handles escaped quotation marks in plain tokens" $
            parseAttributes "foo bar\\\"baz"
                `shouldBe` ( []
                           , Right
                                ( [ PText "foo"
                                  , PText "bar\\\"baz"
                                  ]
                                )
                           )

        it "handles escaped quotation marks in quotations" $
            parseAttributes "foo bar=\"baz\\\"quux\""
                `shouldBe` ( []
                           , Right
                                ( [ PText "foo"
                                  , Assignment "bar" "baz\"quux"
                                  ]
                                )
                           )

        it "handles escaped quotation marks in equalities" $
            parseAttributes "y=\"baz\\\"\""
                `shouldBe` ( []
                           , Right
                                ( [ Assignment "y" "baz\""
                                  ]
                                )
                           )

        it "handles escaped quotation marks in parentheticals" $
            parseAttributes "(foo \\\"bar)"
                `shouldBe` ( []
                           , Right
                                ( [ Parenthetical "foo \\\"bar"
                                  ]
                                )
                           )

        it "handles escaped parentheses in quotations" $
            parseAttributes "foo bar=\"baz\\(quux\""
                `shouldBe` ( []
                           , Right
                                ( [ PText "foo"
                                  , Assignment "bar" "baz(quux"
                                  ]
                                )
                           )

        it "handles escaped parentheses in plain tokens" $
            parseAttributes "foo bar\\(baz"
                `shouldBe` ( []
                           , Right
                                ( [ PText "foo"
                                  , PText "bar(baz"
                                  ]
                                )
                           )

        it "handles escaped parentheses in parentheticals" $
            parseAttributes "(foo \\(bar)"
                `shouldBe` ( []
                           , Right
                                ( [ Parenthetical "foo (bar"
                                  ]
                                )
                           )

        it "handles escaped parentheses in equalities" $
            parseAttributes "y=baz\\("
                `shouldBe` ( []
                           , Right
                                ( [ Assignment "y" "baz("
                                  ]
                                )
                           )

        it "handles mid-token quote in later token" $
            parseAttributes "foo bar baz=(bin\")"
                `shouldBe` ( []
                           , Right
                                ( [ PText "foo"
                                  , PText "bar"
                                  , Assignment "baz" "bin\""
                                  ]
                                )
                           )

    describe "entity field parsing" $ do
        let
            parseField :: String -> ParseResult ()
            parseField s = do
                let
                    (warnings, res) = runConfiguredParser defaultPersistSettings initialExtraState entityField "" s
                case res of
                    Left peb ->
                        (warnings, Left peb)
                    Right (_, _) -> (warnings, Right ())

        it "should error if quotes are unterminated in an attribute" $ do
            (fmap . first) errorBundlePretty (parseField "field String sql=\"foo bar")
                `shouldBe` ( []
                           , Left
                                ( "1:17:\n  |\n1 | field String sql=\"foo bar\n  |                 ^\nunexpected '='\nexpecting '!', '\"', ''', ',', '-', '.', ':', '[', '\\', ']', '_', '~', alphanumeric character, assignment expression, end of input, newline, parenthetical, or plain attribute\n"
                                )
                           )

        it "should error if quotes are unterminated in a type" $ do
            (fmap . first) errorBundlePretty (parseField "field (Label \"unterminated)")
                `shouldBe` ( []
                           , Left
                                ( "1:28:\n  |\n1 | field (Label \"unterminated)\n  |                            ^\nunexpected end of input\nexpecting '\"' or literal character\n"
                                )
                           )

    describe "tab error level setting" $ do
        let
            definitions = T.pack "User\n\tId Text\n\tname String"

        describe "when configured to permit tabs" $ do
            let
                (warnings, [user]) = defsWithWarnings lowerCaseSettings{psTabErrorLevel = Nothing} definitions

            it "permits tab indentation" $
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"

        describe "when configured to warn on tabs" $ do
            let
                (warnings, [user]) =
                    defsWithWarnings
                        lowerCaseSettings{psTabErrorLevel = Just LevelWarning}
                        definitions

            it "permits tab indentation" $
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"

        describe "when configured to disallow tabs" $ do
            let
                [user] =
                    defsWithSettings
                        lowerCaseSettings{psTabErrorLevel = Just LevelError}
                        definitions

            it "rejects tab indentation" $
                evaluate (unboundEntityDef user)
                    `shouldErrorWithMessage` "2:1:\n  |\n2 |  Id Text\n  | ^\nunexpected tab\nexpecting valid whitespace\n\n3:1:\n  |\n3 |  name String\n  | ^\nunexpected tab\nexpecting valid whitespace\n"

    describe "quoted attribute error level setting" $ do
        let
            definitions = T.pack "User\n name String \"Maybe\""

        describe "when configured to warn on quoted attributes" $ do
            let
                (warnings, [user]) =
                    defsWithWarnings
                        lowerCaseSettings{psQuotedArgumentErrorLevel = Just LevelWarning}
                        definitions

            it "permits quoted attributes" $
                (unboundFieldAttrs <$> unboundEntityFields user) `shouldBe` [[FieldAttrMaybe]]

        describe "when configured to disallow quoted attributes" $ do
            let
                (warnings, [user]) =
                    defsWithWarnings
                        lowerCaseSettings{psQuotedArgumentErrorLevel = Just LevelError}
                        definitions

            it "rejects quoted attributes" $
                evaluate (unboundEntityDef user)
                    `shouldErrorWithMessage` "2:14:\n  |\n2 |  name String \"Maybe\"\n  |              ^\nUnexpected quotation mark in field or directive attribute\n"

            describe "and the definition has quotation marks in the type" $ do
                let
                    definitionsWithTypeLevelString = T.pack "User\n name \"String\"\n deriving Show"
                    (warnings2, [user]) =
                        defsWithWarnings
                            lowerCaseSettings{psQuotedArgumentErrorLevel = Just LevelError}
                            definitionsWithTypeLevelString
                it "parses successfully" $
                    getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"

    describe "quoted directive argument error level setting" $ do
        let
            definitions = T.pack "User\n name String\n deriving \"Show\""

        describe "when configured to warn on quoted arguments" $ do
            let
                (warnings, [user]) =
                    defsWithWarnings
                        lowerCaseSettings{psQuotedArgumentErrorLevel = Just LevelWarning}
                        definitions

            it "permits quoted attributes" $
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"

        describe "when configured to disallow quoted arguments" $ do
            let
                (warnings, [user]) =
                    defsWithWarnings
                        lowerCaseSettings{psQuotedArgumentErrorLevel = Just LevelError}
                        definitions

            it "rejects quoted arguments" $
                evaluate (unboundEntityDef user)
                    `shouldErrorWithMessage` "3:11:\n  |\n3 |  deriving \"Show\"\n  |           ^\nUnexpected quotation mark in field or directive attribute\n"

    describe "parse" $ do
        let
            subject =
                [st|
Bicycle -- | this is a bike
    brand String -- | the brand of the bike
    ExtraBike
        foo bar  -- | this is a foo bar
        baz
    deriving Eq
-- | This is a Car
Car
    -- | the make of the Car
    make String
    -- | the model of the Car
    model String
    UniqueModel model
    deriving Eq Show
+Vehicle
    bicycle BicycleId -- | the bike reference
    car CarId         -- | the car reference

                    |]
        let
            [bicycle, car, vehicle] = defs subject

        it "should parse the `entityHaskell` field" $ do
            getUnboundEntityNameHS bicycle `shouldBe` EntityNameHS "Bicycle"
            getUnboundEntityNameHS car `shouldBe` EntityNameHS "Car"
            getUnboundEntityNameHS vehicle `shouldBe` EntityNameHS "Vehicle"

        it "should parse the `entityDB` field" $ do
            entityDB (unboundEntityDef bicycle) `shouldBe` EntityNameDB "bicycle"
            entityDB (unboundEntityDef car) `shouldBe` EntityNameDB "car"
            entityDB (unboundEntityDef vehicle) `shouldBe` EntityNameDB "vehicle"

        it "should parse the `entityAttrs` field" $ do
            entityAttrs (unboundEntityDef bicycle) `shouldBe` []
            entityAttrs (unboundEntityDef car) `shouldBe` []
            entityAttrs (unboundEntityDef vehicle) `shouldBe` []

        it "should parse the `unboundEntityFields` field" $ do
            let
                simplifyField field =
                    (unboundFieldNameHS field, unboundFieldNameDB field, unboundFieldComments field)
            (simplifyField <$> unboundEntityFields bicycle)
                `shouldBe` [ (FieldNameHS "brand", FieldNameDB "brand", Nothing)
                           ]
            (simplifyField <$> unboundEntityFields car)
                `shouldBe` [ (FieldNameHS "make", FieldNameDB "make", Just "the make of the Car\n")
                           , (FieldNameHS "model", FieldNameDB "model", Just "the model of the Car\n")
                           ]
            (simplifyField <$> unboundEntityFields vehicle)
                `shouldBe` [ (FieldNameHS "bicycle", FieldNameDB "bicycle", Nothing)
                           , (FieldNameHS "car", FieldNameDB "car", Nothing)
                           ]

        it "should parse the `entityUniques` field" $ do
            let
                simplifyUnique unique =
                    (uniqueHaskell unique, uniqueFields unique)
            (simplifyUnique <$> entityUniques (unboundEntityDef bicycle)) `shouldBe` []
            (simplifyUnique <$> entityUniques (unboundEntityDef car))
                `shouldBe` [ (ConstraintNameHS "UniqueModel", [(FieldNameHS "model", FieldNameDB "model")])
                           ]
            (simplifyUnique <$> entityUniques (unboundEntityDef vehicle)) `shouldBe` []

        it "should parse quoted attributes" $ do
            let
                [precompiledCacheParent] =
                    defs
                        [st|
                                                   PrecompiledCacheParent sql="precompiled_cache"
                                                     platformGhcDir FilePath "default=(hex(randomblob(16)))"
                                                     deriving Show
                                                |]
            (unboundFieldAttrs <$> unboundEntityFields precompiledCacheParent)
                `shouldBe` [[FieldAttrDefault "(hex(randomblob(16)))"]]

        it "should parse entity block attributes with nested parens on equality rhs" $ do
            let
                [precompiledCacheParent] =
                    defs
                        [st|
                                                   PrecompiledCacheParent sql="precompiled_cache"
                                                     platformGhcDir FilePath default=(hex(randomblob(16)))
                                                |]
            (unboundFieldAttrs <$> unboundEntityFields precompiledCacheParent)
                `shouldBe` [[FieldAttrDefault "hex(randomblob(16))"]]

        it "should parse the `entityForeigns` field" $ do
            let
                [user, notification] =
                    defs
                        [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text
    !yes            Int
    ~no             Int

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text

    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst emailSecond
|]
            unboundForeignDefs user `shouldBe` []
            map unboundForeignDef (unboundForeignDefs notification)
                `shouldBe` [ ForeignDef
                                { foreignRefTableHaskell = EntityNameHS "User"
                                , foreignRefTableDBName = EntityNameDB "user"
                                , foreignConstraintNameHaskell = ConstraintNameHS "fk_noti_user"
                                , foreignConstraintNameDBName = ConstraintNameDB "notificationfk_noti_user"
                                , foreignFieldCascade = FieldCascade Nothing Nothing
                                , foreignFields =
                                    []
                                , -- the foreign fields are not set yet in an unbound
                                  -- entity def
                                  foreignAttrs = []
                                , foreignNullable = False
                                , foreignToPrimary = False
                                }
                           ]

        it "should parse the `entityDerives` field" $ do
            entityDerives (unboundEntityDef bicycle) `shouldBe` ["Eq"]
            entityDerives (unboundEntityDef car) `shouldBe` ["Eq", "Show"]
            entityDerives (unboundEntityDef vehicle) `shouldBe` []

        it "should parse the `entityEntities` field" $ do
            entityExtra (unboundEntityDef bicycle)
                `shouldBe` Map.singleton "ExtraBike" [["foo", "bar"], ["baz"]]
            entityExtra (unboundEntityDef car) `shouldBe` mempty
            entityExtra (unboundEntityDef vehicle) `shouldBe` mempty

        it "should parse the `entitySum` field" $ do
            entitySum (unboundEntityDef bicycle) `shouldBe` False
            entitySum (unboundEntityDef car) `shouldBe` False
            entitySum (unboundEntityDef vehicle) `shouldBe` True

        it "should parse the `entityComments` field" $ do
            entityComments (unboundEntityDef bicycle) `shouldBe` Nothing
            entityComments (unboundEntityDef car) `shouldBe` Just "This is a Car\n"
            entityComments (unboundEntityDef vehicle) `shouldBe` Nothing

        it "should error on malformed input, unterminated parens" $ do
            let
                definitions =
                    [st|
User
    name Text
    age  (Maybe Int
|]
            let
                [user] = defs definitions
            evaluate (unboundEntityDef user)
                `shouldErrorWithMessage` "4:20:\n  |\n4 |     age  (Maybe Int\n  |                    ^\nunexpected newline\nexpecting ''', ')', '.', alphanumeric character, type expression, or white space\n"

        it "errors on duplicate cascade update declarations" $ do
            let
                definitions =
                    [st|
User
    age  Int OnUpdateCascade OnUpdateCascade
|]
            let
                [user] = defs definitions
            mapM (evaluate . unboundFieldCascade) (unboundEntityFields user)
                `shouldErrorWithMessage` "found more than one OnUpdate action, tokens: [\"OnUpdateCascade\",\"OnUpdateCascade\"]"

        it "errors on duplicate cascade delete declarations" $ do
            let
                definitions =
                    [st|
User
    age  Int OnDeleteCascade OnDeleteCascade
|]
            let
                [user] = defs definitions
            mapM (evaluate . unboundFieldCascade) (unboundEntityFields user)
                `shouldErrorWithMessage` "found more than one OnDelete action, tokens: [\"OnDeleteCascade\",\"OnDeleteCascade\"]"

        describe "custom Id column" $ do
            it "parses custom Id column" $ do
                let
                    definitions =
                        [st|
User
    Id   Text
    name Text
    age  Int
|]
                let
                    [user] = defs definitions
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"
                entityDB (unboundEntityDef user) `shouldBe` EntityNameDB "user"
                let
                    idFields = NEL.toList (entitiesPrimary (unboundEntityDef user))
                (fieldHaskell <$> idFields) `shouldBe` [FieldNameHS "Id"]
                (fieldDB <$> idFields) `shouldBe` [FieldNameDB "id"]
                (fieldType <$> idFields) `shouldBe` [FTTypeCon Nothing "Text"]
                (unboundFieldNameHS <$> unboundEntityFields user)
                    `shouldBe` [ FieldNameHS "name"
                               , FieldNameHS "age"
                               ]

            it "errors on duplicate custom Id column" $ do
                let
                    definitions =
                        [st|
User
    Id   Text
    Id   Text
    name Text
    age  Int
|]
                let
                    [user] = defs definitions
                    errMsg = [st|expected only one Id declaration per entity|]
                evaluate (unboundEntityDef user)
                    `shouldErrorWithMessage` (T.unpack errMsg)

        describe "primary declaration" $ do
            it "parses Primary declaration" $ do
                let
                    definitions =
                        [st|
User
    ref Text
    name Text
    age  Int
    Primary ref
|]
                let
                    [user] = defs definitions
                getUnboundEntityNameHS user `shouldBe` EntityNameHS "User"
                entityDB (unboundEntityDef user) `shouldBe` EntityNameDB "user"
                let
                    idFields = NEL.toList (entitiesPrimary (unboundEntityDef user))
                (fieldHaskell <$> idFields) `shouldBe` [FieldNameHS "Id"]
                (fieldDB <$> idFields) `shouldBe` [FieldNameDB "id"]
                (fieldType <$> idFields) `shouldBe` [FTTypeCon Nothing "UserId"]
                (unboundFieldNameHS <$> unboundEntityFields user)
                    `shouldBe` [ FieldNameHS "ref"
                               , FieldNameHS "name"
                               , FieldNameHS "age"
                               ]
                entityUniques (unboundEntityDef user)
                    `shouldBe` [ UniqueDef
                                    { uniqueHaskell =
                                        ConstraintNameHS "UserPrimaryKey"
                                    , uniqueDBName =
                                        ConstraintNameDB "primary_key"
                                    , uniqueFields =
                                        pure (FieldNameHS "ref", FieldNameDB "ref")
                                    , uniqueAttrs =
                                        []
                                    }
                               ]

            it "errors on duplicate custom Primary declaration" $ do
                let
                    definitions =
                        [st|
User
    ref Text
    name Text
    age  Int
    Primary ref
    Primary name
|]
                let
                    [user] = defs definitions
                    errMsg = "expected only one Primary declaration per entity"
                evaluate (unboundEntityDef user)
                    `shouldErrorWithMessage` errMsg

            it "errors on conflicting Primary/Id declarations" $ do
                let
                    definitions =
                        [st|
User
    Id Text
    ref Text
    name Text
    age  Int
    Primary ref
|]
                let
                    [user] = defs definitions
                    errMsg = [st|Specified both an ID field and a Primary field|]
                evaluate (unboundEntityDef user)
                    `shouldErrorWithMessage` (T.unpack errMsg)

            it "triggers error on invalid declaration" $ do
                let
                    definitions =
                        [st|
User
    age Text
    Primary ref
|]
                let
                    [user] = defs definitions
                case unboundPrimarySpec user of
                    NaturalKey ucd -> do
                        evaluate (NEL.head $ unboundCompositeCols ucd)
                            `shouldErrorWithMessage` "Unknown column in primary key constraint: \"ref\""
                    _ ->
                        error "Expected NaturalKey, failing"

        describe "entity unique constraints" $ do
            it "triggers error if declared field does not exist" $ do
                let
                    definitions =
                        [st|
User
    name            Text
    emailFirst      Text

    UniqueEmail emailFirst emailSecond
|]
                let
                    [user] = defs definitions
                    uniques = entityUniques (unboundEntityDef user)
                    [dbNames] = fmap snd . uniqueFields <$> uniques
                    errMsg =
                        unwords
                            [ "Unknown column in \"UniqueEmail\" constraint: \"emailSecond\""
                            , "possible fields: [\"name\",\"emailFirst\"]"
                            ]
                evaluate (head (NEL.tail dbNames))
                    `shouldErrorWithMessage` errMsg

            it "triggers error if no valid constraint name provided" $ do
                let
                    definitions =
                        [st|
User
    age Text
    Unique some
|]
                let
                    [user] = defs definitions
                evaluate (unboundPrimarySpec user)
                    `shouldErrorWithMessage` "invalid unique constraint on table[\"User\"] expecting an uppercase constraint name xs=[\"some\"]"

        describe "foreign keys" $ do
            let
                validDefinitions =
                    [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text

    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst emailSecond
|]

            it "should allow you to modify the FK name via provided function" $ do
                let
                    flippedFK (EntityNameHS entName) (ConstraintNameHS conName) =
                        conName <> entName
                    [_user, notification] = defsWithSettings (setPsToFKName flippedFK lowerCaseSettings) validDefinitions
                    [notificationForeignDef] =
                        unboundForeignDef <$> unboundForeignDefs notification
                foreignConstraintNameDBName notificationForeignDef
                    `shouldBe` ConstraintNameDB "fk_noti_user_notification"

            it "should error when insufficient params provided" $ do
                let
                    definitions =
                        [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text
    Foreign User
|]
                let
                    [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage` "invalid foreign key constraint on table[\"Notification\"] expecting a lower case constraint name or a cascading action xs=[]"

            it "should error when foreign fields not provided" $ do
                let
                    definitions =
                        [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text
    Foreign User fk_noti_user
|]
                let
                    [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage` "No fields on foreign reference."

            it "should error when number of parent and foreign fields differ" $ do
                let
                    definitions =
                        [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text
    Foreign User fk_noti_user sentToFirst sentToSecond References emailFirst
|]
                let
                    [_user, notification] = defsSnake definitions
                mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                    `shouldErrorWithMessage` "invalid foreign key constraint on table[\"Notification\"] Found 2 foreign fields but 1 parent fields"

            it
                "should throw error when there is more than one delete cascade on the declaration"
                $ do
                    let
                        definitions =
                            [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text
    Foreign User OnDeleteCascade OnDeleteCascade
|]
                    let
                        [_user, notification] = defsSnake definitions
                    mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                        `shouldErrorWithMessage` "invalid foreign key constraint on table[\"Notification\"] found more than one OnDelete actions"

            it
                "should throw error when there is more than one update cascade on the declaration"
                $ do
                    let
                        definitions =
                            [st|
User
    name            Text
    emailFirst      Text
    emailSecond     Text

    UniqueEmail emailFirst emailSecond

Notification
    content         Text
    sentToFirst     Text
    sentToSecond    Text
    Foreign User OnUpdateCascade OnUpdateCascade
|]
                    let
                        [_user, notification] = defsSnake definitions
                    mapM (evaluate . unboundForeignFields) (unboundForeignDefs notification)
                        `shouldErrorWithMessage` "invalid foreign key constraint on table[\"Notification\"] found more than one OnUpdate actions"

            it
                "should allow you to enable snake cased foriegn keys via a preset configuration function"
                $ do
                    let
                        [_user, notification] =
                            defsSnake validDefinitions
                        [notificationForeignDef] =
                            unboundForeignDef <$> unboundForeignDefs notification
                    foreignConstraintNameDBName notificationForeignDef
                        `shouldBe` ConstraintNameDB "notification_fk_noti_user"

        describe "ticked types" $ do
            it "should be able to parse ticked types" $ do
                let
                    simplifyField field =
                        (unboundFieldNameHS field, unboundFieldType field)
                let
                    tickedDefinition =
                        [st|
CustomerTransfer
    customerId CustomerId
    moneyAmount (MoneyAmount 'Customer 'Debit)
    currencyCode CurrencyCode
    uuid TransferUuid
|]
                let
                    [customerTransfer] = defs tickedDefinition
                let
                    expectedType =
                        FTTypeCon Nothing "MoneyAmount"
                            `FTApp` FTTypePromoted "Customer"
                            `FTApp` FTTypePromoted "Debit"

                (simplifyField <$> unboundEntityFields customerTransfer)
                    `shouldBe` [ (FieldNameHS "customerId", FTTypeCon Nothing "CustomerId")
                               , (FieldNameHS "moneyAmount", expectedType)
                               , (FieldNameHS "currencyCode", FTTypeCon Nothing "CurrencyCode")
                               , (FieldNameHS "uuid", FTTypeCon Nothing "TransferUuid")
                               ]

        describe "type literals" $ do
            it "should be able to parse type literals" $ do
                let
                    simplifyField field =
                        (unboundFieldNameHS field, unboundFieldType field)
                let
                    tickedDefinition =
                        [st|
WithFinite
    one    (Finite 1)
    twenty (Labelled "twenty")
|]
                let
                    [withFinite] = defs tickedDefinition

                (simplifyField <$> unboundEntityFields withFinite)
                    `shouldBe` [ (FieldNameHS "one", FTApp (FTTypeCon Nothing "Finite") (FTLit (IntTypeLit 1)))
                               ,
                                   ( FieldNameHS "twenty"
                                   , FTApp (FTTypeCon Nothing "Labelled") (FTLit (TextTypeLit "twenty"))
                                   )
                               ]

    describe "parseFieldType" $ do
        it "simple types" $
            parseFieldType "FooBar" `shouldBe` Right (FTTypeCon Nothing "FooBar")
        it "module types" $
            parseFieldType "Data.Map.FooBar"
                `shouldBe` Right (FTTypeCon (Just "Data.Map") "FooBar")
        it "application" $
            parseFieldType "Foo Bar"
                `shouldBe` Right
                    (FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
        it "application multiple" $
            parseFieldType "Foo Bar Baz"
                `shouldBe` Right
                    ( (FTTypeCon Nothing "Foo" `FTApp` FTTypeCon Nothing "Bar")
                        `FTApp` FTTypeCon Nothing "Baz"
                    )
        it "parens" $ do
            let
                foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo (Bar Baz)"
                `shouldBe` Right
                    (foo `FTApp` (bar `FTApp` baz))
        it "lists" $ do
            let
                foo = FTTypeCon Nothing "Foo"
                bar = FTTypeCon Nothing "Bar"
                bars = FTList bar
                baz = FTTypeCon Nothing "Baz"
            parseFieldType "Foo [Bar] Baz"
                `shouldBe` Right
                    (foo `FTApp` bars `FTApp` baz)
        it "numeric type literals" $ do
            let
                expected = FTApp (FTTypeCon Nothing "Finite") (FTLit (IntTypeLit 1))
            parseFieldType "Finite 1" `shouldBe` Right expected
        it "string type literals" $ do
            let
                expected = FTApp (FTTypeCon Nothing "Labelled") (FTLit (TextTypeLit "twenty"))
            parseFieldType "Labelled \"twenty\"" `shouldBe` Right expected
        it "nested list / parens (list inside parens)" $ do
            let
                maybeCon = FTTypeCon Nothing "Maybe"
                int = FTTypeCon Nothing "Int"
            parseFieldType "Maybe (Maybe [Int])"
                `shouldBe` Right
                    (maybeCon `FTApp` (maybeCon `FTApp` FTList int))
        it "nested list / parens (parens inside list)" $ do
            let
                maybeCon = FTTypeCon Nothing "Maybe"
                int = FTTypeCon Nothing "Int"
            parseFieldType "[Maybe (Maybe Int)]"
                `shouldBe` Right
                    (FTList (maybeCon `FTApp` (maybeCon `FTApp` int)))
        it "fails on lowercase starts" $ do
            parseFieldType "nothanks" `shouldBe` Left "PSFail \"nothanks\""

    describe "#1175 empty entity" $ do
        let
            subject =
                [st|
Foo
    name String
    age Int

EmptyEntity

Bar
    name String

Baz
    a Int
    b String
    c FooId
                    |]

        it "parse works" $ do
            let
                test name'fieldCount parsedList = do
                    case (name'fieldCount, parsedList) of
                        ([], []) ->
                            pure ()
                        ((name, fieldCount) : _, []) ->
                            expectationFailure $
                                "Expected an entity with name "
                                    <> name
                                    <> " and "
                                    <> show fieldCount
                                    <> " fields"
                                    <> ", but the list was empty..."
                        ((name, fieldCount) : ys, (x : xs)) -> do
                            let
                                UnboundEntityDef{..} =
                                    x
                            (unEntityNameHS (getUnboundEntityNameHS x), length unboundEntityFields)
                                `shouldBe` (T.pack name, fieldCount)
                            test ys xs
                        ([], _ : _) ->
                            expectationFailure
                                "more entities parsed than expected"

                result =
                    defs subject
            length result `shouldBe` 4

            test
                [ ("Foo", 2)
                , ("EmptyEntity", 0)
                , ("Bar", 1)
                , ("Baz", 3)
                ]
                result

arbitraryWhiteSpaceChar :: Gen Char
arbitraryWhiteSpaceChar =
    oneof $ pure <$> [' ', '\t', '\n', '\r']

shouldErrorWithMessage :: IO a -> String -> Expectation
shouldErrorWithMessage action expectedMsg = do
    res <- try action
    case res of
        Left (ErrorCall msg) ->
            msg `shouldBe` expectedMsg
        _ ->
            expectationFailure "Expected `error` to have been called"
