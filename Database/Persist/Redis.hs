{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Database.Persist.Redis
    ( RedisReader
    , runRedis
    , persistRedis
    , IsStrings (..)
    ) where

import Database.Persist (Persist, Table (..), Key, Order, Filter, Update, Unique)
import Database.Persist.Helper
import Control.Monad.Trans.Reader
import qualified Data.Map as Map
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char
import Control.Arrow (first)
import Data.List (sortBy, intercalate)
import Data.Maybe (mapMaybe, fromJust)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Control.Applicative (Applicative)
import Data.Convertible (Convertible (..))
import Control.Monad
import qualified Database.Redis.Redis as R
import Data.Attempt
import Data.Convertible.Text
import Data.Typeable
import Control.Exception (Exception)
import Safe

persistRedis = fmap concat . mapM derive

runRedis = runReaderT

type RedisReader = ReaderT R.Redis

class IsStrings a where
    toStrings :: a -> [String]
    fromStrings :: [String] -> Attempt a

derive :: Table -> Q [Dec]
derive t@(Table name cols upda filts ords uni) = do
    let dt = dataTypeDec t
    let monad = ConT ''RedisReader `AppT` VarT (mkName "m")

    tsv <- mkToStrings t
    fsv <- mkFromStrings t
    let sq =
          InstanceD [] (ConT ''IsStrings `AppT` ConT (mkName name))
            [ FunD (mkName "toStrings") [tsv]
            , FunD (mkName "fromStrings") fsv
            ]

    let keysyn = TySynD (mkName $ name ++ "Id") [] $
                    ConT ''Key `AppT` ConT (mkName name)

    t' <- TH.lift t
    let mkFun s e = FunD (mkName s) [Clause [] (NormalB $ e `AppE` t') []]

    init' <- [|initialize|]
    select' <- [|select|]
    insert' <- [|insert|]
    {-
    getBy' <- [|getBy|]
    insertR' <- [|insertR|]
    replace' <- [|replace|]
    get' <- [|get|]
    deleteWhere' <- [|deleteWhere|]
    delete' <- [|delete|]
    deleteBy' <- [|deleteBy|]
    update' <- [|update|]
    updateWhere' <- [|updateWhere|]
    -}

    let inst =
          InstanceD
            [ClassP ''MonadCatchIO [VarT $ mkName "m"]]
            (ConT ''Persist `AppT` ConT (mkName name) `AppT` monad)
            [ keyTypeDec (name ++ "Id") "Integer" t monad
            , filterTypeDec t monad
            , updateTypeDec t monad
            , orderTypeDec t monad
            , uniqueTypeDec t monad
            , mkFun "initialize" $ init'
            , mkFun "select" $ select'
            , mkFun "insert" $ insert'
            {-
            , mkFun "getBy" $ getBy'
            , mkFun "insertR" $ insertR'
            , mkFun "replace" $ replace'
            , mkFun "get" $ get'
            , mkFun "deleteWhere" $ deleteWhere'
            , mkFun "delete" $ delete'
            , mkFun "deleteBy" $ deleteBy'
            , mkFun "update" $ update'
            , mkFun "updateWhere" $ updateWhere'
            -}
            ]
    return [dt, sq, inst, keysyn]

mkToStrings :: Table -> Q Clause
mkToStrings t = do
    xs <- mapM (const $ newName "x") $ tableColumns t
    ts <- [|cs|]
    let xs' = map (AppE ts . VarE) xs
    return $ Clause [ConP (mkName $ tableName t) $ map VarP xs]
                    (NormalB $ ListE xs') []

data InvalidFromStrings = InvalidFromStrings
    deriving (Show, Typeable)
instance Exception InvalidFromStrings

mkFromStrings :: Table -> Q [Clause]
mkFromStrings t = do
    nothing <- [|Failure InvalidFromStrings|]
    let cons = ConE $ mkName $ tableName t
    xs <- mapM (const $ newName "x") $ tableColumns t
    fs <- [|ca|]
    let xs' = map (AppE fs . VarE) xs
    let pat = ListP $ map VarP xs
    ap' <- [|ap|]
    just <- [|Success|]
    let cons' = just `AppE` cons
    return
        [ Clause [pat] (NormalB $ foldl (go ap') cons' xs') []
        , Clause [WildP] (NormalB nothing) []
        ]
  where
    go ap' x y = InfixE (Just x) ap' (Just y)

initialize _ _ = return ()

select t _ _ = do
    r <- ask
    R.RMulti (Just s) <- liftIO $ R.smembers r (tableName t ++ ":all")
    let s' = map (\(R.RBulk (Just x)) -> x :: Int) s
    vals <- liftIO $ mapM (\x -> R.get r $ tableName t ++ ":id:" ++ show x) s'
    let vals' = map (\(R.RBulk (Just x)) -> x :: String) vals
    return $ mapMaybe go $ zip s' vals'
  where
    go (k, v) = do
        v' <- readMay v
        v'' <- fa $ fromStrings v'
        return (fromIntegral k, v'')

insert t val = do
    let val' = show $ toStrings val
    r <- ask
    R.RInt i <- liftIO $ R.incr r $ "global:" ++ tableName t ++ ":nextId"
    liftIO $ print i
    liftIO $ R.set r (tableName t ++ ":id:" ++ show i) val'
    --R.set r (tableName t ++ ":slug:" ++ show i) i
    liftIO $ R.sadd r (tableName t ++ ":all") i
    return $ fromIntegral i
