{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.Query.Join.Sql
    ( RunJoin (..)
    ) where

import Database.Persist.Query.Join hiding (RunJoin (..))
import Database.Persist.EntityDef
import qualified Database.Persist.Query.Join as J
import Database.Persist.Store
import Database.Persist.Query
import Database.Persist.Query.GenericSql
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
import Data.Text (Text, pack, concat, append, null)
import Control.Monad.IO.Class (MonadIO)
import qualified Prelude
import Prelude hiding ((++), unlines, concat, show, null)
import Data.Monoid (Monoid, mappend)
import qualified Data.Text as T

fromPersistValuesId :: PersistEntity v => [PersistValue] -> Either Text (Key SqlPersist v, v)
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
                        (Left e, _) -> error $ "selectOneMany: " ++ T.unpack e
                        (Right y', Left e) ->
                            case z of
                                PersistNull:_ -> loop (front . (:) (y', Nothing)) popper
                                _ -> error $ "selectOneMany: " ++ T.unpack e
        oneCount = 1 + length (entityFields $ entityDef one)
        one = dummyFromFilts oneF
        many = dummyFromFilts manyF
        sql conn = concat
            [ "SELECT "
            , T.intercalate "," $ colsPlusId conn one ++ colsPlusId conn many
            , " FROM "
            , escapeName conn $ entityDB $ entityDef one
            , if isOuter then " LEFT JOIN " else " INNER JOIN "
            , escapeName conn $ entityDB $ entityDef many
            , " ON "
            , escapeName conn $ entityDB $ entityDef one
            , ".id = "
            , escapeName conn $ entityDB $ entityDef many
            , "."
            , escapeName conn $ filterName $ eq undefined
            , filts
            , case ords of
                [] -> ""
                _ -> " ORDER BY " ++ T.intercalate ", " ords
            ]
          where
            filts1 = filterClauseNoWhere True conn oneF
            filts2 = filterClauseNoWhere True conn manyF

            orders :: PersistEntity val
                   => [SelectOpt val]
                   -> [SelectOpt val]
            orders x = let (_, _, y) = limitOffsetOrder x in y

            filts
                | null filts1 && null filts2 = ""
                | null filts1 = " WHERE " ++ filts2
                | null filts2 = " WHERE " ++ filts1
                | otherwise = " WHERE " ++ filts1 ++ " AND " ++ filts2
            ords = map (orderClause True conn) (orders oneO) ++ map (orderClause True conn) (orders manyO)

addTable :: PersistEntity val
         => Connection
         -> val
         -> Text
         -> Text
addTable conn e s = concat
    [ escapeName conn $ entityDB $ entityDef e
    , "."
    , s
    ]

colsPlusId :: PersistEntity e => Connection -> e -> [Text]
colsPlusId conn e =
    map (addTable conn e) $
    id_ : (map (escapeName conn . fieldDB) cols)
  where
    id_ = escapeName conn $ entityID $ entityDef e
    cols = entityFields $ entityDef e

filterName :: PersistEntity v => Filter v -> DBName
filterName (Filter f _ _) = fieldDB $ persistFieldDef f
filterName (FilterAnd _) = error "expected a raw filter, not an And"
filterName (FilterOr _) = error "expected a raw filter, not an Or"

infixr 5 ++
(++) :: Monoid m => m -> m -> m
(++) = mappend
