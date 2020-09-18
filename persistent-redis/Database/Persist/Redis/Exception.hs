module Database.Persist.Redis.Exception
    ( RedisException (..)
    ) where

import Control.Exception (Exception)

data RedisException = NotSupportedOperation String
                    | ParserError String
                    | NotSupportedValueType String
                    | IncorrectUpdate String
                    | IncorrectBehavior

instance Show RedisException where
    show (NotSupportedOperation key) = "The operation is not supported: " ++ key
    show (ParserError msg) = "Error during parsing: " ++ msg
    show (NotSupportedValueType tp) = "The value type " ++ tp ++ " is not supported for Redis"
    show IncorrectBehavior = "The behavior of persistent-redis is incorrect"
    show (IncorrectUpdate msg) = "This update is not possible: " ++ msg
instance Exception RedisException
