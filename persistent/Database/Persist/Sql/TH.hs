{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Database.Persist.Sql.TH
  ( fromN
  , generateRawSqlHelper
  , generateRawSqlInstances
  , toN
  )
where

import Control.Monad
import Language.Haskell.TH

#if MIN_VERSION_template_haskell(2,16,0)
prepTupE :: a -> Maybe a
prepTupE = Just
#else
prepTupE :: a -> a
prepTupE = id
#endif

minTupleSize :: Int
minTupleSize = 3 -- instance for 2 is already written

maxTupleSize :: Int
maxTupleSize = 62 -- GHC limitation

pairedList :: [a] -> [[a]]
pairedList [] = []
pairedList [x] = [[x]]
pairedList (x:y:rest) = [x, y] : pairedList rest

flatTupleT :: [Name] -> Type
flatTupleT names = foldl1 AppT $ (TupleT $ length names) : map VarT names

nestedTupleT :: [Name] -> Type
nestedTupleT names =
   foldl1 AppT $ (TupleT $ (length names + 1) `div` 2) : map tupleOrSingleT (pairedList names)

tupleOrSingleT :: [Name] -> Type
tupleOrSingleT [a, b] = AppT (AppT (TupleT 2) (VarT a)) (VarT b)
tupleOrSingleT [a] = VarT a
tupleOrSingleT x = error $ "tupleOrSingleT failed to convert: " <> show x

fromN :: Int -> Q (Type, Exp)
fromN n = do
    names <- replicateM n $ newName "x"
    let arg = [TupP . map VarP $ names]
    pure ( AppT (AppT ArrowT $ flatTupleT names) (nestedTupleT names)
         , LamE arg . TupE . map (prepTupE . tupleOrSingleE) $ pairedList names
         )

    where
        tupleOrSingleE :: [Name] -> Exp
        tupleOrSingleE [a, b] = TupE $ map (prepTupE . VarE) [a, b]
        tupleOrSingleE [a] = VarE a
        tupleOrSingleE x = error $ "tupleOrSingleE failed to convert: " <> show x

toN :: Int -> Q (Type, Exp)
toN n = do
    names <- replicateM n $ newName "x"
    let arg = [TupP . map tupleOrSingleP $ pairedList names]
    pure ( AppT (AppT ArrowT $ nestedTupleT names) (flatTupleT names)
         , LamE arg . TupE $ map (prepTupE . VarE) names
         )

    where
        tupleOrSingleP :: [Name] -> Pat
        tupleOrSingleP [a, b] = TupP $ map VarP [a, b]
        tupleOrSingleP [a] = VarP a
        tupleOrSingleP x = error $ "tupleOrSingleP failed to convert: " <> show x

rawSqlInstanceN :: Int -> Q Dec
rawSqlInstanceN n = do
    names <- replicateM n $ newName "x"

    let constraints = map (\name -> AppT (ConT $ mkName "RawSql") (VarT name)) names
        tuple = AppT (ConT $ mkName "RawSql") . foldl1 AppT $ (TupleT $ length names) : map VarT names

    pure $ InstanceD Nothing constraints tuple
        [ FunD (mkName "rawSqlCols") [Clause [] (NormalB rawSqlColsBody) []]
        , FunD (mkName "rawSqlColCountReason") [Clause [] (NormalB rawSqlColCountReasonBody) []]
        , FunD (mkName "rawSqlProcessRow") [Clause [] (NormalB rawSqlProcessRowBody) []]
        ]

    where
        rawSqlColsBody :: Exp
        rawSqlColsBody =
            LamE [VarP $ mkName "e"]
                 (InfixE (Just $ AppE (VarE $ mkName "rawSqlCols") (VarE $ mkName "e"))
                         (VarE $ mkName ".")
                         (Just . VarE $ mkName $ "from" ++ show n))

        rawSqlColCountReasonBody :: Exp
        rawSqlColCountReasonBody =
            (InfixE (Just . VarE $ mkName "rawSqlColCountReason")
                    (VarE $ mkName ".")
                    (Just . VarE $ mkName $ "from" ++ show n))

        rawSqlProcessRowBody :: Exp
        rawSqlProcessRowBody =
            (InfixE (Just $ AppE (VarE 'fmap) (VarE . mkName $ "to" ++ show n))
                    (VarE $ mkName ".")
                    (Just . VarE $ mkName "rawSqlProcessRow"))

generateRawSqlHelper :: String -> (Int -> Q (Type, Exp)) -> Q [Dec]
generateRawSqlHelper funcName func =
    fmap concat $ forM [minTupleSize..maxTupleSize] $ \tupleSize -> do
        (sig, body) <- func tupleSize
        let name = mkName $ funcName ++ show tupleSize
        pure [ SigD name sig
             , FunD name [Clause [] (NormalB body) []]
             ]

generateRawSqlInstances :: Q [Dec]
generateRawSqlInstances =
    forM [minTupleSize..maxTupleSize] $ \tupleSize -> do
        rawSqlInstanceN tupleSize
