{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module Database.Persist.Query.Internal
  ( -- re-exported as public
      PersistQuery (..)
    , selectList
    , selectKeysList

    -- just Internal
    , SelectOpt (..)
    , limitOffsetOrder
    , Filter (..)
    , PersistUpdate (..)
    , Update (..)
    , updateFieldDef
    , deleteCascadeWhere

    -- * Exceptions
    , UpdateGetException (..)

    -- * Filters
    , BackendSpecificFilter
  ) where

import Database.Persist.Store
import Database.Persist.Types
import Database.Persist.Class.PersistQuery
import Database.Persist.Class.PersistEntity
import Database.Persist.EntityDef
import Control.Monad.IO.Class (liftIO)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

import Control.Monad.Trans.Class (lift)
import Data.Monoid (Monoid)

import Data.Conduit.Internal (Pipe, ConduitM)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error   )
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import Control.Monad.Trans.Resource ( ResourceT)

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

-- | Call 'selectSource' but return the result as a list.
selectList :: (PersistEntity val, PersistQuery m, PersistEntityBackend val ~ PersistMonadBackend m)
           => [Filter val]
           -> [SelectOpt val]
           -> m [Entity val]
selectList a b = selectSource a b C.$$ CL.consume

-- | Call 'selectKeys' but return the result as a list.
selectKeysList :: (PersistEntity val, PersistQuery m, PersistEntityBackend val ~ PersistMonadBackend m)
               => [Filter val]
               -> [SelectOpt val]
               -> m [Key val]
selectKeysList a b = selectKeys a b C.$$ CL.consume

limitOffsetOrder :: PersistEntity val => [SelectOpt val] -> (Int, Int, [SelectOpt val])
limitOffsetOrder opts =
    foldr go (0, 0, []) opts
  where
    go (LimitTo l) (_, b, c) = (l, b ,c)
    go (OffsetBy o) (a, _, c) = (a, o, c)
    go x (a, b, c) = (a, b, x : c)

updateFieldDef :: PersistEntity v => Update v -> FieldDef
updateFieldDef (Update f _ _) = persistFieldDef f

deleteCascadeWhere :: (DeleteCascade a m, PersistQuery m)
                   => [Filter a] -> m ()
deleteCascadeWhere filts = selectKeys filts [] C.$$ CL.mapM_ deleteCascade
