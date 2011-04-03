{-# LANGUAGE FlexibleContexts #-}
module Database.Persist.Join.Sql
    ( selectOneMany
    ) where

import Database.Persist.Base
import Control.Monad (forM, liftM)
import Data.Maybe (catMaybes)
import Data.List (intercalate, groupBy)
import Database.Persist.GenericSql (SqlPersist (SqlPersist))
import Database.Persist.GenericSql.Internal hiding (withStmt)
import Database.Persist.GenericSql.Raw (withStmt)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Control (MonadControlIO)
import Data.Function (on)
import Control.Arrow ((&&&))

fromPersistValuesId :: PersistEntity v => [PersistValue] -> Either String (Key v, v)
fromPersistValuesId [] = Left "fromPersistValuesId: No values provided"
fromPersistValuesId (i:rest) =
    case fromPersistValues rest of
        Left e -> Left e
        Right x -> Right (toPersistKey i, x)

selectOneMany :: (PersistEntity one, PersistEntity many, MonadControlIO m, Eq (Key one))
              => [Filter one]  -> [Order one]
              -> [Filter many] -> [Order many]
              -> (Key one -> Filter many)
              -> SqlPersist m [((Key one, one), [(Key many, many)])]
selectOneMany oneF oneO manyF manyO eq = do
    conn <- SqlPersist ask
    liftM go $ withStmt (sql conn) (getFiltsValues oneF ++ getFiltsValues manyF) $ loop id
  where
    go :: Eq a => [((a, b), (c, d))] -> [((a, b), [(c, d)])]
    go = map (fst . head &&& map snd) . groupBy ((==) `on` (fst . fst))
    loop front popper = do
        x <- popper
        case x of
            Nothing -> return $ front []
            Just vals -> do
                let (y, z) = splitAt oneCount vals
                case (fromPersistValuesId y, fromPersistValuesId z) of
                    (Right y', Right z') -> loop (front . (:) (y', z')) popper
                    (Left e, _) -> error $ "selectOneMany: " ++ e
                    (_, Left e) -> error $ "selectOneMany: " ++ e
    oneCount = 1 + length (tableColumns $ entityDef one)
    one = dummyFromFilts oneF
    many = dummyFromFilts manyF
    sql conn = concat
        [ "SELECT "
        , intercalate "," $ colsPlusId conn one ++ colsPlusId conn many
        , " FROM "
        , escapeName conn $ rawTableName $ entityDef one
        , " INNER JOIN "
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

addTable conn e s = concat [escapeName conn $ rawTableName $ entityDef e, ".", s]

--colsPlusId :: PersistEntity e => e -> [String]
colsPlusId conn e =
    map (addTable conn e) $
    "id" : (map (\(x, _, _) -> escapeName conn x) cols)
  where
    cols = tableColumns $ entityDef e
