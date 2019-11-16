{-# LANGUAGE PatternGuards #-}
-- | Intended for creating new backends.
module Database.Persist.Sql.Internal
    ( mkColumns
    , defaultAttribute
    ) where

import Data.Char (isSpace)
import Data.Monoid (mappend, mconcat)
import Data.Text (Text)
import qualified Data.Text as T

import Database.Persist.Quasi
import Database.Persist.Sql.Types
import Database.Persist.Types

defaultAttribute :: [Attr] -> Maybe Text
defaultAttribute [] = Nothing
defaultAttribute (a:as)
    | Just d <- T.stripPrefix "default=" a = Just d
    | otherwise = defaultAttribute as

-- | Create the list of columns for the given entity.
mkColumns :: [EntityDef] -> EntityDef -> ([Column], [UniqueDef], [ForeignDef])
mkColumns allDefs t =
    (cols, entityUniques t, entityForeigns t)
  where
    cols :: [Column]
    cols = map go (entityFields t)

    tn :: DBName
    tn = entityDB t

    go :: FieldDef -> Column
    go fd =
        Column
            (fieldDB fd)
            (nullable (fieldAttrs fd) /= NotNullable || entitySum t)
            (fieldSqlType fd)
            (defaultAttribute $ fieldAttrs fd)
            Nothing
            (maxLen $ fieldAttrs fd)
            (ref (fieldDB fd) (fieldReference fd) (fieldAttrs fd))

    maxLen :: [Attr] -> Maybe Integer
    maxLen [] = Nothing
    maxLen (a:as)
        | Just d <- T.stripPrefix "maxlen=" a =
            case reads (T.unpack d) of
              [(i, s)] | all isSpace s -> Just i
              _ -> error $ "Could not parse maxlen field with value " ++
                           show d ++ " on " ++ show tn
        | otherwise = maxLen as

    ref :: DBName
        -> ReferenceDef
        -> [Attr]
        -> Maybe (DBName, DBName) -- table name, constraint name
    ref c fe []
        | ForeignRef f _ <- fe =
            Just (resolveTableName allDefs f, refName tn c)
        | otherwise = Nothing
    ref _ _ ("noreference":_) = Nothing
    ref c fe (a:as)
        | Just x <- T.stripPrefix "reference=" a = do
            constraintName <- snd <$> (ref c fe as)
            pure (DBName x, constraintName)
        | Just x <- T.stripPrefix "constraint=" a = do
            tableName <- fst <$> (ref c fe as)
            pure (tableName, DBName x)
    ref c x (_:as) = ref c x as

refName :: DBName -> DBName -> DBName
refName (DBName table) (DBName column) =
    DBName $ Data.Monoid.mconcat [table, "_", column, "_fkey"]

resolveTableName :: [EntityDef] -> HaskellName -> DBName
resolveTableName [] (HaskellName hn) = error $ "Table not found: " `Data.Monoid.mappend` T.unpack hn
resolveTableName (e:es) hn
    | entityHaskell e == hn = entityDB e
    | otherwise = resolveTableName es hn
