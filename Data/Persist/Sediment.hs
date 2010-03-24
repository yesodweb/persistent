{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Persist.Sediment
    ( RockBeds (..)
    ) where

import Data.Persist
import Data.Sediment
import System.Directory
import Data.Serialize

data RockBeds = RockBeds FilePath

instance Sediment [(String, FieldValue)] where
    type Erosion [(String, FieldValue)] = ()
    erode = const
instance Serialize FieldValue where
    put (FVString s) = putWord8 0 >> put s
    put (FVInt i) = putWord8 1 >> put i
    get = do
        x <- getWord8
        case x of
            0 -> FVString `fmap` get
            1 -> FVInt `fmap` get
            _ -> fail $ "get FieldValue invalid flag: " ++ show x

instance DataStore RockBeds where
    type RecordId RockBeds = RockId
    initTable (RockBeds fp) table =
        createDirectoryIfMissing True $ fp ++ '/' : tableName table
    createRecord (RockBeds fp) table rec = do
        let rb = RockBed $ fp ++ '/' : tableName table
        newRock rb $ tableFreeze table rec
    readRecord (RockBeds fp) table rid = do
        res <- readRock rb rid
        case res of
            Left _ -> return Nothing
            Right (res', _) -> return $ tableThaw table res'
      where
        rb = RockBed $ fp ++ '/' : tableName table
    updateRecord (RockBeds fp) table rid rec =
        replaceRock rb rid $ tableFreeze table rec
      where
        rb = RockBed $ fp ++ '/' : tableName table
    deleteRecord (RockBeds fp) table rid = deleteRock rb rid where
        rb = RockBed $ fp ++ '/' : tableName table
    filterTable (RockBeds fp) table filters iter a0 = do
        allRocks <- rockList rb
        go allRocks a0
      where
        rb = RockBed $ fp ++ '/' : tableName table
        go [] a = return $ Right a
        go (rid:rids) a = do
            res <- readRock rb rid
            case res of
                Left _ -> go rids a
                Right (res', _) -> do
                    if applyFilters res' filters
                        then case tableThaw table res' of
                                Nothing -> go rids a
                                Just x -> do
                                    ea <- iter (rid, x) a
                                    case ea of
                                        Left a' -> return $ Left a'
                                        Right a' -> go rids a'
                        else go rids a
