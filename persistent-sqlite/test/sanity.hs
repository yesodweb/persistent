{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Database.Persist.Sqlite

$(return []) -- just force TH to run

main :: IO ()
main = withSqliteConn ":memory:" $ const $ return ()
