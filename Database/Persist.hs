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
type Column = (String, (String, Bool)) -- is it nullable?

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
    data Key    val
    data Update val
    data Filter val
    data Order  val
    data Unique val

    -- initialization, value is ignored
    initialize  :: val                          -> m ()

    -- insert
    insert      :: val                          -> m (Key val)
    insertR     :: val                          -> m (Key val)
    replace     :: Key val      -> val          -> m ()

    -- modify
    update      :: Key val      -> [Update val] -> m ()
    updateWhere :: [Filter val] -> [Update val] -> m ()

    -- delete
    delete      :: Key val                      -> m ()
    deleteBy    :: Unique val                   -> m ()
    deleteWhere :: [Filter val]                 -> m ()

    -- read single
    get         :: Key val                      -> m (Maybe val)
    getBy       :: Unique val                   -> m (Maybe (Key val, val))

    -- read multiple
    select      :: [Filter val] -> [Order val]  -> m [(Key val, val)]
