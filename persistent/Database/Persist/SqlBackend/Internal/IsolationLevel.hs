module Database.Persist.SqlBackend.Internal.IsolationLevel where

import Data.String (IsString(..))

-- | Please refer to the documentation for the database in question for a full
-- overview of the semantics of the varying isloation levels
data IsolationLevel = ReadUncommitted
                    | ReadCommitted
                    | RepeatableRead
                    | Serializable
                    deriving (Show, Eq, Enum, Ord, Bounded)

makeIsolationLevelStatement :: (Monoid s, IsString s) => IsolationLevel -> s
makeIsolationLevelStatement l = "SET TRANSACTION ISOLATION LEVEL " <> case l of
    ReadUncommitted -> "READ UNCOMMITTED"
    ReadCommitted -> "READ COMMITTED"
    RepeatableRead -> "REPEATABLE READ"
    Serializable -> "SERIALIZABLE"
