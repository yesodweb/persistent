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
import Control.Monad.Trans.State hiding (get)
import qualified Control.Monad.Trans.State as S
import qualified Data.Map as Map
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char
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

recName :: String -> String -> String
recName dt f = map toLower dt ++ upperFirst f

derivePersistState :: Table -> Q [Dec]
derivePersistState (Table name cols upda filts ords uni) = do
    let name' = mkName name
    let cols' = map (mkCol name) cols
    let dt = DataD [] name' [] [RecC name' cols'] []
    let monad = ConT ''PersistState `AppT` ConT name' `AppT` VarT (mkName "m")
    let key = NewtypeInstD [] ''Key [ConT name', monad]
                (NormalC (mkName "StateKey") [(NotStrict, ConT ''Int)])
                [''Show, ''Read, ''Num, ''Integral, ''Enum, ''Eq, ''Ord,
                 ''Real]
    let fil = DataInstD [] ''Filter [ConT name', monad]
                (concatMap (mkFilter name cols) filts)
                [''Show, ''Read, ''Eq]
    let upd = DataInstD [] ''Update [ConT name', monad]
                (map (mkUpdate name cols) upda)
                [''Show, ''Read, ''Eq]
    let ord = DataInstD [] ''Order [ConT name', monad]
                (concatMap (mkOrder name) ords)
                [''Show, ''Read, ''Eq]
    insert'' <- [|insert'|]
    replace'' <- [|replace'|]
    yget <- [|xget|]
    ydelete <- [|xdelete|]

    yfilter' <- [|xfilter|]
    af <- mkApplyFilter name cols filts
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

mkCol :: String -> Column -> VarStrictType
mkCol x (n, t) =
    (mkName $ recName x n, NotStrict, ConT $ mkName t)

filtsToList :: (String, Bool, Bool, Bool, Bool, Bool, Bool)
            -> [(String, String)]
filtsToList (s, a, b, c, d, e, f)
    = go $ zip ["Eq", "Ne", "Gt", "Lt", "Ge", "Le"] [a, b, c, d, e, f]
  where
    go [] = []
    go ((_, False):rest) = go rest
    go ((x, True):rest) = (s, x) : go rest

mkFilter :: String
         -> [(String, String)]
         -> (String, Bool, Bool, Bool, Bool, Bool, Bool)
         -> [Con]
mkFilter x cols filts = map go $ filtsToList filts
  where
    go (s, t) = NormalC (mkName $ x ++ upperFirst s ++ t)
                               [(NotStrict, ConT $ mkName $ ty s)]
    ty s = case lookup s cols of
                Nothing -> error $ "Invalid column: " ++ s
                Just ty' -> ty'

mkApplyFilter base cols filts = do
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

mkUpdate :: String -> [(String, String)] -> String -> Con
mkUpdate x cols s =
    NormalC (mkName $ x ++ upperFirst s)
                [(NotStrict, ConT $ mkName ty)]
  where
    ty = case lookup s cols of
                Nothing -> error $ "Invalid column: " ++ s
                Just ty' -> ty'

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

mkOrder :: String -> (String, Bool, Bool) -> [Con]
mkOrder x (s, a, d) =
     (if a then (:) (go "Asc") else id)
   $ (if d then (:) (go "Desc") else id) []
  where
    go ad = NormalC (mkName $ x ++ upperFirst s ++ ad) []

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

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs
upperFirst [] = []

insert' val = do
    m <- get'
    let k :: Int
        k = 1 + Map.foldrWithKey (\k _ k' -> max k k') 0 m
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
