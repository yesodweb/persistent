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

import Database.Persist (PersistEntity, Key)
import Database.Persist.Helper
import Control.Monad.Trans.Reader
import Language.Haskell.TH.Syntax hiding (lift)
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (mapMaybe)
import "MonadCatchIO-transformers" Control.Monad.CatchIO
import Control.Monad
import qualified Database.Redis.Redis as R
import Data.Attempt
import Data.Convertible.Text
import Data.Typeable
import Safe

persistRedis :: [EntityDef] -> Q [Dec]
persistRedis = fmap concat . mapM derive

runRedis :: RedisReader m a -> R.Redis -> m a
runRedis = runReaderT

type RedisReader = ReaderT R.Redis

class IsStrings a where
    toStrings :: a -> [String]
    fromStrings :: [String] -> Attempt a

derive :: EntityDef -> Q [Dec]
derive t = do
    let name = entityName t
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
            (ConT ''PersistEntity `AppT` ConT (mkName name) `AppT` monad)
            [ keyTypeDec (name ++ "Id") "Integer" t
            , filterTypeDec t
            , updateTypeDec t
            , orderTypeDec t
            , uniqueTypeDec t
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

mkToStrings :: EntityDef -> Q Clause
mkToStrings t = do
    xs <- mapM (const $ newName "x") $ entityColumns t
    ts <- [|cs|]
    let xs' = map (AppE ts . VarE) xs
    return $ Clause [ConP (mkName $ entityName t) $ map VarP xs]
                    (NormalB $ ListE xs') []

data InvalidFromStrings = InvalidFromStrings
    deriving (Show, Typeable)
instance Exception InvalidFromStrings

mkFromStrings :: EntityDef -> Q [Clause]
mkFromStrings t = do
    nothing <- [|Failure InvalidFromStrings|]
    let cons = ConE $ mkName $ entityName t
    xs <- mapM (const $ newName "x") $ entityColumns t
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

initialize :: Monad m => x -> y -> m ()
initialize _ _ = return ()

select :: (IsStrings v, Num (Key v), MonadIO m)
       => EntityDef -> x -> y -> RedisReader m [(Key v, v)]
select t _ _ = do
    r <- ask
    R.RMulti (Just s) <- liftIO $ R.smembers r (entityName t ++ ":all")
    let s' = map (\(R.RBulk (Just x)) -> x :: Int) s
    vals <- liftIO $ mapM (\x -> R.get r $ entityName t ++ ":id:" ++ show x) s'
    let vals' = map (\(R.RBulk (Just x)) -> x :: String) vals
    return $ mapMaybe go $ zip s' vals'
  where
    go (k, v) = do
        v' <- readMay v
        v'' <- fa $ fromStrings v'
        return (fromIntegral k, v'')

insert :: (IsStrings a, MonadIO m, Num (Key a))
       => EntityDef -> a -> RedisReader m (Key a)
insert t val = do
    let val' = show $ toStrings val
    r <- ask
    R.RInt i <- liftIO $ R.incr r $ "global:" ++ entityName t ++ ":nextId"
    liftIO $ print i
    _ <- liftIO $ R.set r (entityName t ++ ":id:" ++ show i) val'
    --R.set r (entityName t ++ ":slug:" ++ show i) i
    _ <- liftIO $ R.sadd r (entityName t ++ ":all") i
    return $ fromIntegral i
