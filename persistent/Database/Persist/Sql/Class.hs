{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
module Database.Persist.Sql.Class where

import Database.Persist.Sql.Types

import Data.Monoid (Monoid)
import Control.Monad.Trans.Class (lift)

import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Cont     ( ContT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import Control.Monad.Trans.Reader   ( ReaderT, ask  )
import Control.Monad.Trans.Resource ( ResourceT )
import Data.Conduit.Internal (Pipe, ConduitM)

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Logger (MonadLogger)

class (MonadIO m, MonadLogger m) => MonadSqlPersist m where
    askSqlConn :: m Connection
    default askSqlConn :: (MonadSqlPersist m, MonadTrans t, MonadLogger (t m))
                       => t m Connection
    askSqlConn = lift askSqlConn

instance (MonadIO m, MonadLogger m) => MonadSqlPersist (SqlPersist m) where
    askSqlConn = SqlPersist ask

#define GO(T) instance (MonadSqlPersist m) => MonadSqlPersist (T m)
#define GOX(X, T) instance (X, MonadSqlPersist m) => MonadSqlPersist (T m)
GO(LoggingT)
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(ContT r)
GO(StateT s)
GO(ResourceT)
GO(Pipe l i o u)
GO(ConduitM i o)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
#undef GO
#undef GOX
