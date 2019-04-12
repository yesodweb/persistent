{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad.Logger

import Database.Persist.Sqlite

$(return []) -- just force TH to run

main :: IO ()
main = runStderrLoggingT $ withSqliteConn ":memory:" $ runSqlConn waitForDatabase
