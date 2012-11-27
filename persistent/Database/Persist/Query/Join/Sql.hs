{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Persist.Query.Join.Sql
    ( RunJoin (..)
    ) where

import Database.Persist.Query.Join hiding (RunJoin (..))
import Database.Persist.EntityDef
import qualified Database.Persist.Query.Join as J
import Database.Persist.Store
import Database.Persist.Query.Internal
import Database.Persist.Query.GenericSql
import Control.Monad (liftM)
import Data.Maybe (mapMaybe)
import Data.List (groupBy)
import Database.Persist.GenericSql
import Database.Persist.GenericSql.Internal hiding (withStmt)
import Database.Persist.GenericSql.Raw (withStmt, MonadSqlPersist, askSqlConn)
import Data.Function (on)
import Control.Arrow ((&&&))
import Data.Text (Text, concat, null)
import Prelude hiding ((++), unlines, concat, show, null)
import Data.Monoid (Monoid, mappend)
import qualified Data.Text as T
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.Logger (MonadLogger)

fromPersistValuesId :: PersistEntity v => [PersistValue] -> Either Text (Entity v)
fromPersistValuesId [] = Left "fromPersistValuesId: No values provided"
fromPersistValuesId (PersistInt64 i:rest) =
    case fromPersistValues rest of
        Left e -> Left e
        Right x -> Right (Entity (Key $ PersistInt64 i) x)
fromPersistValuesId _ = Left "fromPersistValuesId: invalid ID"

class RunJoin a where
    runJoin :: (C.MonadResource m, MonadLogger m, MonadSqlPersist m) => a -> m (J.Result a)

instance (PersistEntity one, PersistEntity many, Eq (Key one))
    => RunJoin (SelectOneMany one many) where
    runJoin (SelectOneMany oneF oneO manyF manyO eq _getKey isOuter) = do
        conn <- askSqlConn
        liftM go $ withStmt (sql conn) (getFiltsValues conn oneF ++ getFiltsValues conn manyF) C.$$ loop id
      where
        go :: [(Entity b, Maybe (Entity d))]
           -> [(Entity b, [Entity d])]
        go = map (fst . head &&& mapMaybe snd)
           . groupBy ((==) `on` (entityKey . fst))

        loop front = do
            x <- CL.head
            case x of
                Nothing -> return $ front []
                Just vals -> do
                    let (y, z) = splitAt oneCount vals
                    case (fromPersistValuesId y, fromPersistValuesId z) of
                        (Right y', Right z') -> loop (front . (:) (y', Just z'))
                        (Left e, _) -> error $ "selectOneMany: " ++ T.unpack e
                        (Right y', Left e) ->
                            case z of
                                PersistNull:_ -> loop (front . (:) (y', Nothing))
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
            , onFilts
            , whereFilts
            , case ords of
                [] -> ""
                _ -> " ORDER BY " ++ T.intercalate ", " ords
            ]
          where
            filts1 = filterClauseNoWhere True conn oneF
            filts2 = (if isOuter then filterClauseNoWhereOrNull else filterClauseNoWhere) True conn manyF

            whereFilts
                | isOuter =
                    if null filts1
                        then ""
                        else " WHERE " ++ filts1
                | null filts1 && null filts2 = ""
                | null filts1 = " WHERE " ++ filts2
                | null filts2 = " WHERE " ++ filts1
                | otherwise = " WHERE " ++ filts1 ++ " AND " ++ filts2

            onFilts
                | isOuter && not (null filts2) = " AND " ++ filts2
                | otherwise = ""

            orders :: PersistEntity val
                   => [SelectOpt val]
                   -> [SelectOpt val]
            orders x = let (_, _, y) = limitOffsetOrder x in y

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
filterName (BackendFilter _) = error "expected a raw filter, not a BackendFilter"

infixr 5 ++
(++) :: Monoid m => m -> m -> m
(++) = mappend
