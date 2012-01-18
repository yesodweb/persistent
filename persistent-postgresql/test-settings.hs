import Database.Persist.Postgresql
import Database.Persist.Store
import Data.Yaml

main :: IO ()
main = do
    Just yaml <- decodeFile "settings.yaml"
    conf <- parseMonad loadConfig yaml
    conf' <- applyEnv (conf :: PostgresConf)
    pool <- createPoolConfig conf'
    runPool conf' (return ()) pool
