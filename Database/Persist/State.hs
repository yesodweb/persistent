{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Persist.State
    ( PersistState
    , runPersistState
    , evalPersistState
    , derivePersistState
    ) where

import Database.Persist hiding (filter)
import Database.Persist.Helper
import Control.Monad.Trans.State hiding (get)
import qualified Control.Monad.Trans.State as S
import qualified Data.Map as Map
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Arrow (first)
import Data.List (sortBy)

newtype PersistState val m a = PersistState
    { unPersistState :: StateT (Map.Map Int val) m a
    }
    deriving Monad

get' :: Monad m => PersistState val m (Map.Map Int val)
get' = PersistState S.get

put' :: Monad m => Map.Map Int val -> PersistState val m ()
put' = PersistState . put

instance MonadIO m => MonadIO (PersistState val m) where
    liftIO = lift . liftIO
instance MonadTrans (PersistState val) where
    lift = PersistState . lift

runPersistState :: PersistState val m a -> Map.Map Int val
                -> m (a, Map.Map Int val)
runPersistState = runStateT . unPersistState

evalPersistState :: Monad m
                 => PersistState val m a -> Map.Map Int val
                 -> m (a)
evalPersistState = evalStateT . unPersistState

derivePersistState :: Table -> Q [Dec]
derivePersistState t@(Table name _cols upda filts ords _uni) = do
    let name' = mkName name
    let dt = dataTypeDec t
    let monad = ConT ''PersistState `AppT` ConT name' `AppT` VarT (mkName "m")
    let key = keyTypeDec "StateKey" "Int" t monad
    let fil = filterTypeDec t monad
    let upd = updateTypeDec t monad
    let ord = orderTypeDec t monad
    insert'' <- [|insert'|]
    replace'' <- [|replace'|]
    yget <- [|xget|]
    ydelete <- [|xdelete|]

    yfilter' <- [|xfilter|]
    af <- mkApplyFilter name filts
    let yfilter = yfilter' `AppE` af

    ydw' <- [|xdeleteWhere|]
    let ydw = ydw' `AppE` af

    yupdate' <- [|xupdate|]
    au <- mkApplyUpdate name upda
    let yupdate = yupdate' `AppE` au

    yuw' <- [|xupdateWhere|]
    let yuw = yuw' `AppE` af `AppE` au

    yorder' <- [|xorder|]
    ao <- mkApplyOrds name ords
    let yorder = yorder' `AppE` ao

    yselect' <- [|xselect|]
    let yselect = yselect' `AppE` af `AppE` ao

    let inst =
          InstanceD
            [ClassP ''Monad [VarT $ mkName "m"]]
            (ConT ''Persist `AppT` ConT name' `AppT` monad)
            [ key, fil, upd, ord
            , FunD (mkName "insert") [Clause [] (NormalB insert'') []]
            , FunD (mkName "replace") [Clause [] (NormalB replace'') []]
            , FunD (mkName "get") [Clause [] (NormalB yget) []]
            , FunD (mkName "delete") [Clause [] (NormalB ydelete) []]
            , FunD (mkName "filter") [Clause [] (NormalB yfilter) []]
            , FunD (mkName "deleteWhere") [Clause [] (NormalB ydw) []]
            , FunD (mkName "update") [Clause [] (NormalB yupdate) []]
            , FunD (mkName "updateWhere") [Clause [] (NormalB yuw) []]
            , FunD (mkName "order") [Clause [] (NormalB yorder) []]
            , FunD (mkName "select") [Clause [] (NormalB yselect) []]
            ]
    return [dt, inst]

mkApplyFilter base filts = do
    v <- newName "v"
    f <- newName "f"
    return $ LamE [VarP v, VarP f] $ CaseE (VarE f) $ concatMap (go v) filts
  where
    go v filt = map (go' v) $ filtsToList filt
    go' v (rec, com) =
        Match (ConP (mkName $ base ++ upperFirst rec ++ com)
                                 [VarP $ mkName "x"])
              (NormalB $ (com' com) `AppE`
                          (VarE (mkName $ recName base rec) `AppE` VarE v) `AppE`
                          VarE (mkName "x")
                         )
              []
    com' "Eq" = VarE $ mkName "=="
    com' "Ne" = VarE $ mkName "/="
    com' "Lt" = VarE $ mkName "<"
    com' "Gt" = VarE $ mkName ">"
    com' "Le" = VarE $ mkName "<="
    com' "Ge" = VarE $ mkName ">="
    com' x = error $ "invalid com': " ++ x

mkApplyUpdate :: String -> [String] -> Q Exp
mkApplyUpdate base upda = do
    v <- newName "v"
    u <- newName "u"
    return $ LamE [VarP u, VarP v] $ CaseE (VarE u) $ map (go v) upda
  where
    go v u = Match (ConP (mkName $ base ++ upperFirst u)
                            [VarP $ mkName "x"])
                   (NormalB $ RecUpdE (VarE v)
                        [(mkName $ recName base u, VarE $ mkName "x")])
                   []

mkApplyOrds :: String -> [(String, Bool, Bool)] -> Q Exp
mkApplyOrds base ords = do
    x <- newName "x"
    y <- newName "y"
    o <- newName "o"
    return $ LamE [VarP o, VarP x, VarP y] $ CaseE (VarE o)
        $ concatMap (go (VarE x) (VarE y)) ords
  where
    go x y (r, a, d) =
        (if a then (:) (go' x y r True "Asc") else id)
      $ (if d then (:) (go' x y r False "Desc") else id) []
    go' x y r isAsc suf =
        let x' = VarE (mkName (recName base r)) `AppE` x
            y' = VarE (mkName (recName base r)) `AppE` y
         in Match (ConP (mkName $ base ++ upperFirst r ++ suf) [])
              (NormalB $
                if isAsc
                    then (VarE (mkName "compare") `AppE` x' `AppE` y')
                    else (VarE (mkName "compare") `AppE` y' `AppE` x')) []

insert' val = do
    m <- get'
    let k :: Int
        k = 1 + Map.foldrWithKey (\k1 _ k2 -> max k1 k2) 0 m
    put' $ Map.insert k val m
    return $ fromIntegral k

replace' k val = do
    m <- get'
    put' $ Map.insert (fromIntegral k) val m

xget k = do
    m <- get'
    return $ Map.lookup (fromIntegral k) m

xdelete k = do
    m <- get'
    put' $ Map.delete (fromIntegral k) m

xfilter af filts = do
    m <- get'
    return $ map (first fromIntegral)
           $ filter (\(_, v) -> all (af v) filts) $ Map.toList m

xdeleteWhere af filts = do
    m <- get'
    put' $ Map.fromList $ filter (\(_, v) -> not $ all (af v) filts)
         $ Map.toList m

xupdate au k ups = do
    m <- get'
    put' $ case Map.lookup (fromIntegral k) m of
            Nothing -> m
            Just v -> Map.insert (fromIntegral k) (foldr au v ups) m

xupdateWhere af au filts ups = do
    m <- get'
    put' $ Map.fromList $ map go $ Map.toList m
  where
    go (k, v) = if all (af v) filts
                    then (k, foldr au v ups)
                    else (k, v)

xorder ao ords = do
    m <- get'
    return $ map (first fromIntegral)
           $ sortBy (\(_, x) (_, y) -> go ords x y)
           $ Map.toList m
  where
    go [] _ _ = EQ
    go (o:os) x y =
        case ao o x y of
            LT -> LT
            GT -> GT
            EQ -> go os x y

xselect af ao filts ords = do
    m <- get'
    return $ map (first fromIntegral)
           $ sortBy (\(_, x) (_, y) -> go ords x y)
           $ filter (\(_, v) -> all (af v) filts)
           $ Map.toList m
  where
    go [] _ _ = EQ
    go (o:os) x y =
        case ao o x y of
            LT -> LT
            GT -> GT
            EQ -> go os x y
