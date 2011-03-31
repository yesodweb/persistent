import Data.Pool
import System.Environment

main = do
    [x] <- getArgs
    createPool (return ()) (const $ return ()) 1000 $ \p -> do
        sequence_ $ replicate (read x) $ withPool' p return
