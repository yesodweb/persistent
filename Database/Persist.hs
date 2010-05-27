{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Persist
    ( -- * High level design
      Column  (..)
    , Table   (..)
      -- * Type class
      Persist (..)
    ) where

data Column = Column
    { columnName :: String
    , columnType :: String
    }

data Table = Table
    { tableName    :: String
    , tableColumns :: [Column]
    , tableUpdates ::
    , tableFilters :: [(String, Bool, Bool, Bool, Bool, Bool, Bool)] -- eq, ne, gt, lt, ge, le
    , tableOrders  :: [(String, Bool, Bool)] -- asc, desc
    , tableUniques :: [[String]]
    }

class Monad m => Persist val m where
    data Key    val m
    data Update val m
    data Filter val m
    data Order  val m
    data Unique val m

    -- write
    insert      :: val                              -> m (Key val m)
    replace     :: Key val m      -> val            -> m (Key val m)
    replaceBy   :: Unique val m   -> val            -> m (Key val m)
    update      :: Key val m      -> [Update val m] -> m ()
    delete      :: Key val m                        -> m ()
    deleteWhere :: [Filter val m]                   -> m ()

    -- read
    get         :: Key val m                        -> m (Maybe val)
    getBy       :: Unique val m                     -> m (Maybe (Key val m, val))
    filter      :: [Filter val m]                   -> m [(Key val m, val)] -- enumerator
    order       :: [Order val m]                    -> m [(Key val m, val)] -- enumerator
    select      :: [Filter val m] -> [Order val m]  -> m [(Key val m, val)] -- enumerator
