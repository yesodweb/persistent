{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Persist.Sql.TH
  ( fromN
  , generateRawSqlHelper
  , generateRawSqlInstances
  , toN
  )
where

import Control.Monad
import Language.Haskell.TH
import Database.Persist.Sql.Types

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
maxTupleSize = 4 -- GHC limitation

data PairedList a
  = Pair a a (PairedList a)
  | End (Maybe a)

pairedList :: [a] -> PairedList a
pairedList [] = End Nothing
pairedList [x] = End $ Just x
pairedList (x:y:rest) = Pair x y $ pairedList rest

flatTupleT :: [Name] -> Type
flatTupleT names = foldl1 AppT $ (TupleT $ length names) : map VarT names

nestedTupleT :: [Name] -> Type
nestedTupleT names =
   foldl1 AppT $ (TupleT $ (length names + 1) `div` 2) : tupleOrSingleT (pairedList names)

tupleOrSingleT :: PairedList Name -> [Type]
tupleOrSingleT (Pair a b rest) = AppT (AppT (TupleT 2) (VarT a)) (VarT b) : tupleOrSingleT rest
tupleOrSingleT (End (Just x)) = [VarT x]
tupleOrSingleT (End Nothing) = []

fromN :: Int -> Q (Type, Exp)
fromN n = do
    names <- replicateM n $ newName "x"
    let arg = [TupP . map VarP $ names]
    pure ( AppT (AppT ArrowT $ flatTupleT names) (nestedTupleT names)
         , LamE arg . TupE . map prepTupE . tupleOrSingleE $ pairedList names
         )

    where
        tupleOrSingleE :: PairedList Name -> [Exp]
        tupleOrSingleE (Pair a b rest) = TupE (map (prepTupE . VarE) [a, b]) : tupleOrSingleE rest
        tupleOrSingleE (End (Just x)) = [VarE x]
        tupleOrSingleE (End Nothing) = []

toN :: Int -> Q (Type, Exp)
toN n = do
    names <- replicateM n $ newName "x"
    let arg = [TupP . tupleOrSingleP $ pairedList names]
    pure ( AppT (AppT ArrowT $ nestedTupleT names) (flatTupleT names)
         , LamE arg . TupE $ map (prepTupE . VarE) names
         )

    where
        tupleOrSingleP :: PairedList Name -> [Pat]
        tupleOrSingleP (Pair a b rest) = TupP [VarP a, VarP b] : tupleOrSingleP rest
        tupleOrSingleP (End (Just x)) = [VarP x]
        tupleOrSingleP (End Nothing) = []

rawSqlInstanceN :: Int -> Q [Dec]
rawSqlInstanceN n = do
    names <- replicateM n $ newName "x"

    let constraints = foldl1 AppT $ (TupleT $ length names) : map (AppT (ConT ''RawSql) . VarT) names

        tuple = AppT (ConT ''RawSql) . foldl1 AppT $ (TupleT $ length names) : map VarT names

    [d|
      instance $(pure constraints) => $(pure tuple) where
          rawSqlCols = \e -> rawSqlCols e . $(fromOrToN From n)
          rawSqlColCountReason = rawSqlColCountReason . $(fromOrToN From n)
          rawSqlProcessRow = fmap $(fromOrToN To n) . rawSqlProcessRow
      |]

data FromOrTo = From | To

fromOrToN :: FromOrTo -> Int -> Q Exp
fromOrToN fromOrTo n = pure . VarE . mkName $ str fromOrTo ++ show n
  where str From = "from"
        str To = "to"

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
    fmap concat $ mapM rawSqlInstanceN [minTupleSize..maxTupleSize]
