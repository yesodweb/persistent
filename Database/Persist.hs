{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Persist
    ( -- * High level design
      Column
    , Table   (..)
      -- * Type class
    , Persist (..)
    ) where

import Language.Haskell.TH.Syntax

-- | name, type
type Column = (String, String)

data Table = Table
    { tableName    :: String
    , tableColumns :: [Column]
    , tableUpdates :: [String]
    , tableFilters :: [(String, Bool, Bool, Bool, Bool, Bool, Bool)] -- eq, ne, gt, lt, ge, le
    , tableOrders  :: [(String, Bool, Bool)] -- asc, desc
    , tableUniques :: [(String, [String])]
    }

instance Lift Table where
    lift (Table a b c d e f) = do
        t <- [|Table|]
        a' <- lift a
        b' <- lift b
        c' <- lift c
        d' <- lift d
        e' <- lift e
        f' <- lift f
        return $ t `AppE` a' `AppE` b' `AppE` c' `AppE` d' `AppE` e' `AppE` f'

class Monad m => Persist val m where
    data Key    val m
    data Update val m
    data Filter val m
    data Order  val m
    data Unique val m

    -- initialization, value is ignored
    initialize  :: val                              -> m ()

    -- write
    insert      :: val                              -> m (Key val m)
    insertR     :: val                              -> m (Key val m)
    replace     :: Key val m      -> val            -> m ()
    update      :: Key val m      -> [Update val m] -> m ()
    updateWhere :: [Filter val m] -> [Update val m] -> m ()
    delete      :: Key val m                        -> m ()
    deleteWhere :: [Filter val m]                   -> m ()

    -- read
    get         :: Key val m                        -> m (Maybe val)
    getBy       :: Unique val m                     -> m (Maybe (Key val m, val))
    filter      :: [Filter val m]                   -> m [(Key val m, val)] -- enumerator
    order       :: [Order val m]                    -> m [(Key val m, val)] -- enumerator
    select      :: [Filter val m] -> [Order val m]  -> m [(Key val m, val)] -- enumerator
