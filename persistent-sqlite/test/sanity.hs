{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Database.Persist.Sqlite
import Control.Monad.Logger

$(return []) -- just force TH to run

main :: IO ()
main = runStderrLoggingT $ withSqliteConn ":memory:" $ const $ return ()
