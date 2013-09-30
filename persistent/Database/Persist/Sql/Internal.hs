{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
-- | Intended for creating new backends.
module Database.Persist.Sql.Internal
    ( mkColumns
    ) where

import Database.Persist.Types
import Database.Persist.Quasi
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid (Monoid, mappend, mconcat)
import Data.Maybe (mapMaybe, listToMaybe)
import Database.Persist.Sql.Types

-- | Create the list of columns for the given entity.
mkColumns :: [EntityDef a] -> EntityDef SqlType -> ([Column], [UniqueDef])
mkColumns allDefs t =
    (cols, entityUniques t)
  where
    cols :: [Column]
    cols = map go (entityFields t)

    tn :: DBName
    tn = entityDB t

    go :: FieldDef SqlType -> Column
    go fd =
        Column
            (fieldDB fd)
            (nullable (fieldAttrs fd) /= NotNullable || entitySum t)
            (maybe
                (fieldSqlType fd)
                SqlOther
                (listToMaybe $ mapMaybe (T.stripPrefix "sqltype=") $ fieldAttrs fd))
            (def $ fieldAttrs fd)
            Nothing
            (maxLen $ fieldAttrs fd)
            (ref (fieldDB fd) (fieldType fd) (fieldAttrs fd))

    def :: [Attr] -> Maybe Text
    def [] = Nothing
    def (a:as)
        | Just d <- T.stripPrefix "default=" a = Just d
        | otherwise = def as

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
        -> FieldType
        -> [Attr]
        -> Maybe (DBName, DBName) -- table name, constraint name
    ref c ft []
        | Just f <- stripId ft =
            Just (resolveTableName allDefs $ HaskellName f, refName tn c)
        | otherwise = Nothing
    ref _ _ ("noreference":_) = Nothing
    ref c _ (a:_)
        | Just x <- T.stripPrefix "reference=" a =
            Just (DBName x, refName tn c)
    ref c x (_:as) = ref c x as

refName :: DBName -> DBName -> DBName
refName (DBName table) (DBName column) =
    DBName $ mconcat [table, "_", column, "_fkey"]

resolveTableName :: [EntityDef a] -> HaskellName -> DBName
resolveTableName [] (HaskellName hn) = error $ "Table not found: " `mappend` T.unpack hn
resolveTableName (e:es) hn
    | entityHaskell e == hn = entityDB e
    | otherwise = resolveTableName es hn
