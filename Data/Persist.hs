{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Persist
    ( -- * Data types
      FieldType (..)
    , Field (..)
    , FieldValue (..)
    , Table (..)
      -- * Querying
    , Filter (..)
    , applyFilter
    , applyFilters
      -- * Type classes
    , DataStore (..)
      -- * Memory store sample implementation
    , MemoryStore
    , createMemoryStore
    , showMemoryStore
    ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Concurrent.MVar
import Safe
import Data.Maybe
import Control.Arrow

data FieldType = FTString | FTInt
    deriving (Show, Read, Data, Typeable)

data Field = Field
    { fieldName :: String
    , fieldType :: FieldType
    --, fieldUnique :: Bool
    --, fieldSearchable :: Bool
    , fieldNullable :: Bool
    }
    deriving (Show, Read, Data, Typeable)

data FieldValue = {-FVNull | -}FVString String | FVInt Int
    deriving (Show, Read, Data, Typeable)

data Table a = Table
    { tableName :: String
    , tableFields :: [Field]
    , tableFreeze :: a -> [(String, FieldValue)]
    , tableThaw :: [(String, FieldValue)] -> Maybe a -- error info?
    }

data Filter = Filter
    { filterFieldName :: String
    , filterValue :: FieldValue
    , filterOrderings :: [Ordering]
    }

compareFV :: FieldValue -> FieldValue -> Ordering
compareFV (FVString x) (FVString y) = compare x y
compareFV (FVInt x) (FVInt y) = compare x y
compareFV x y = error $ "Invalid args to compareFV: " ++ show (x, y)

applyFilter :: [(String, FieldValue)] -> Filter -> Bool
applyFilter vals filter =
    case lookup (filterFieldName filter) vals of
      Nothing -> error $ "applyFilter: field not found: "
                      ++ filterFieldName filter
      Just val -> compareFV val (filterValue filter) `elem`
                            filterOrderings filter

applyFilters :: [(String, FieldValue)] -> [Filter] -> Bool
applyFilters vals = and . map (applyFilter vals)

class (Show (RecordId d), Read (RecordId d)) => DataStore d where
    type RecordId d
    initTable :: d -> Table a -> IO ()
    createRecord :: d -> Table a -> a -> IO (RecordId d)
    readRecord :: d -> Table a -> RecordId d -> IO (Maybe a) -- error info
    updateRecord :: d -> Table a -> RecordId d -> a -> IO ()
    deleteRecord :: d -> Table a -> RecordId d -> IO ()
    filterTable :: d -> Table a -> [Filter] -> IO [(RecordId d, a)]

newtype MemoryStore = MemoryStore
    { unMemoryStore :: MVar [(String, [(Int, String)])]
    }

createMemoryStore :: IO MemoryStore
createMemoryStore = MemoryStore `fmap` newMVar []

showMemoryStore :: MemoryStore -> IO String
showMemoryStore = fmap show . readMVar . unMemoryStore

instance DataStore MemoryStore where
    type RecordId MemoryStore = Int
    initTable (MemoryStore ms) table = modifyMVar_ ms $ return . go where
        go = (:) (tableName table, [])
    createRecord (MemoryStore ms) table rec = modifyMVar ms $ return . go
      where
        go [] = error $ "Table not found: " ++ tableName table
        go ((name, vals):rest)
            | name == tableName table =
                let (vals', rid) = go' vals
                 in ((name, vals') : rest, rid)
            | otherwise =
                let (rest', rid) = go rest
                 in ((name, vals) : rest', rid)
        go' vals =
            let rid = maximum $ 1 : map fst vals
             in ((rid, show $ tableFreeze table rec) : vals, rid)
    readRecord (MemoryStore ms) table rid = go `fmap` readMVar ms where
        go store =
          case lookup (tableName table) store of
            Nothing -> error $ "Table not found: " ++ tableName table
            Just vals -> do
                recString <- lookup rid vals
                fieldVals <- readMay recString
                tableThaw table fieldVals
    updateRecord (MemoryStore ms) table rid rec = modifyMVar_ ms $ return . go
      where
        go [] = error $ "Table not found: " ++ tableName table
        go ((name, vals):rest)
            | name == tableName table = (name, go' vals) : rest
            | otherwise = (name, vals) : go rest
        go' [] = error $ "RecordID not found: " ++ show (tableName table, rid)
        go' ((rid', val):rest)
            | rid == rid' = (rid, show $ tableFreeze table rec) : rest
            | otherwise = (rid', val) : go' rest
    deleteRecord (MemoryStore ms) table rid = modifyMVar_ ms $ return . go
      where
        go [] = error $ "Table not found: " ++ tableName table
        go ((name, vals):rest)
            | name == tableName table = (name, go' vals) : rest
            | otherwise = (name, vals) : go rest
        go' [] = error $ "RecordID not found: " ++ show (tableName table, rid)
        go' ((rid', val):rest)
            | rid == rid' = rest
            | otherwise = (rid', val) : go' rest
    filterTable (MemoryStore ms) table filters = go `fmap` readMVar ms where
        go store =
          case lookup (tableName table) store of
            Nothing -> error $ "Table not found: " ++ tableName table
            Just vals -> mapMaybe (\(x, my) -> my >>= \y -> return (x, y))
                       $ map (second $ tableThaw table)
                       $ filter (flip applyFilters filters . snd)
                       $ map (second read) vals
