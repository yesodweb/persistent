{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Database.Persist.Join.Sql
    ( RunJoin (..)
    ) where

import Database.Persist.Join hiding (RunJoin (..))
import qualified Database.Persist.Join as J
import Database.Persist.Store
import Database.Persist.Query
import Database.Persist.GenericSql.ConvertFilters
import Control.Monad (liftM)
import Data.Maybe (mapMaybe)
import Data.List (intercalate, groupBy)
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Internal hiding (withStmt)
import Database.Persist.GenericSql.Raw (withStmt)
import Control.Monad.Trans.Reader (ask)
#if MIN_VERSION_monad_control(0, 3, 0)
import Control.Monad.Trans.Control (MonadBaseControl)
#define MBCIO MonadBaseControl IO
#else
import Control.Monad.IO.Control (MonadControlIO)
#define MBCIO MonadControlIO
#endif
import Data.Function (on)
import Control.Arrow ((&&&))
import Data.Text (pack)
import Control.Monad.IO.Class (MonadIO)

fromPersistValuesId :: PersistEntity v => [PersistValue] -> Either String (Key SqlPersist v, v)
fromPersistValuesId [] = Left "fromPersistValuesId: No values provided"
fromPersistValuesId (PersistInt64 i:rest) =
    case fromPersistValues rest of
        Left e -> Left e
        Right x -> Right (Key $ PersistInt64 i, x)
fromPersistValuesId _ = Left "fromPersistValuesId: invalid ID"

class RunJoin a where
    runJoin :: (MonadIO m, MBCIO m) => a -> SqlPersist m (J.Result a)

instance (PersistEntity one, PersistEntity many, Eq (Key SqlPersist one))
    => RunJoin (SelectOneMany SqlPersist one many) where
    runJoin (SelectOneMany oneF oneO manyF manyO eq _getKey isOuter) = do
        conn <- SqlPersist ask
        liftM go $ withStmt (sql conn) (getFiltsValues conn oneF ++ getFiltsValues conn manyF) $ loop id
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
            , escapeName conn $ RawName $ filterName $ eq undefined
            , filts
            , if null ords
                then ""
                else " ORDER BY " ++ intercalate ", " ords
            ]
          where
            filts1 = filterClauseNoWhere True conn oneF
            filts2 = filterClauseNoWhere True conn manyF

            orders :: PersistEntity val => [SelectOpt val] -> [SelectOpt val]
            orders = third3 . limitOffsetOrder

            filts
                | null filts1 && null filts2 = ""
                | null filts1 = " WHERE " ++ filts2
                | null filts2 = " WHERE " ++ filts1
                | otherwise = " WHERE " ++ filts1 ++ " AND " ++ filts2
            ords = map (orderClause True conn) (orders oneO) ++ map (orderClause True conn) (orders manyO)

addTable :: PersistEntity val =>
           Connection -> val -> [Char] -> [Char]
addTable conn e s = concat [escapeName conn $ rawTableName $ entityDef e, ".", s]

colsPlusId :: PersistEntity e => Connection -> e -> [String]
colsPlusId conn e =
    map (addTable conn e) $
    id_ : (map (\(x, _, _) -> escapeName conn x) cols)
  where
    id_ = unRawName $ rawTableIdName $ entityDef e
    cols = tableColumns $ entityDef e

filterName :: PersistEntity v => Filter v -> String
filterName (Filter f _ _) = columnName $ persistColumnDef f
filterName (FilterAnd _) = error "expected a raw filter, not an And"
filterName (FilterOr _) = error "expected a raw filter, not an Or"
