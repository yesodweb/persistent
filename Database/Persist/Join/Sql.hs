{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Join.Sql
    ( RunJoin (..)
    ) where

import Database.Persist.Join hiding (RunJoin (..))
import qualified Database.Persist.Join as J
import Database.Persist.Base
import Control.Monad (liftM)
import Data.Maybe (mapMaybe)
import Data.List (intercalate, groupBy)
import Database.Persist.GenericSql (SqlPersist (SqlPersist))
import Database.Persist.GenericSql.Internal hiding (withStmt)
import Database.Persist.GenericSql.Raw (withStmt)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Control (MonadControlIO)
import Data.Function (on)
import Control.Arrow ((&&&))
import Data.Text (pack)

fromPersistValuesId :: PersistEntity v => [PersistValue] -> Either String (Key v, v)
fromPersistValuesId [] = Left "fromPersistValuesId: No values provided"
fromPersistValuesId (i:rest) =
    case fromPersistValues rest of
        Left e -> Left e
        Right x -> Right (toPersistKey i, x)

class RunJoin a where
    runJoin :: MonadControlIO m => a -> SqlPersist m (J.Result a)

instance (PersistEntity one, PersistEntity many, Eq (Key one))
    => RunJoin (SelectOneMany one many) where
    runJoin = selectOneMany'

selectOneMany' :: (MonadControlIO m,
                  PersistEntity d,
                  PersistEntity val1,
                  PersistEntity val,
                  PersistEntity b,
                  Eq (Key b)) =>
                 SelectOneMany val val1 -> SqlPersist m [((Key b, b), [(Key d, d)])]
selectOneMany' (SelectOneMany oneF oneO manyF manyO eq _getKey isOuter) = do
    conn <- SqlPersist ask
    liftM go $ withStmt (sql conn) (getFiltsValues oneF ++ getFiltsValues manyF) $ loop id
  where
    go :: Eq a => [((a, b), Maybe (c, d))] -> [((a, b), [(c, d)])]
    go = map (fst . head &&& mapMaybe snd) . groupBy ((==) `on` (fst . fst))
    loop front popper = do
        x <- popper
        case x of
            Nothing -> return $ front []
            Just vals -> do
                let (y, z) = splitAt oneCount vals
                case (fromPersistValuesId y, fromPersistValuesId z) of
                    (Right y', Right z') -> loop (front . (:) (y', Just z')) popper
                    (Left e, _) -> error $ "selectOneMany: " ++ e
                    (Right y', Left e) ->
                        case z of
                            PersistNull:_ -> loop (front . (:) (y', Nothing)) popper
                            _ -> error $ "selectOneMany: " ++ e
    oneCount = 1 + length (tableColumns $ entityDef one)
    one = dummyFromFilts oneF
    many = dummyFromFilts manyF
    sql conn = pack $ concat
        [ "SELECT "
        , intercalate "," $ colsPlusId conn one ++ colsPlusId conn many
        , " FROM "
        , escapeName conn $ rawTableName $ entityDef one
        , if isOuter then " LEFT JOIN " else " INNER JOIN "
        , escapeName conn $ rawTableName $ entityDef many
        , " ON "
        , escapeName conn $ rawTableName $ entityDef one
        , ".id = "
        , escapeName conn $ rawTableName $ entityDef many
        , "."
        , escapeName conn $ RawName $ persistFilterToFieldName $ eq undefined
        , if null filts
            then ""
            else " WHERE " ++ intercalate " AND " filts
        , if null ords
            then ""
            else " ORDER BY " ++ intercalate ", " ords
        ]
      where
        filts = map (filterClause True conn) oneF ++ map (filterClause True conn) manyF
        ords = map (orderClause True conn) oneO ++ map (orderClause True conn) manyO

addTable :: PersistEntity val =>
           Connection -> val -> [Char] -> [Char]
addTable conn e s = concat [escapeName conn $ rawTableName $ entityDef e, ".", s]

colsPlusId :: PersistEntity e => Connection -> e -> [String]
colsPlusId conn e =
    map (addTable conn e) $
    "id" : (map (\(x, _, _) -> escapeName conn x) cols)
  where
    cols = tableColumns $ entityDef e
