import PersistentTest
import RenameTest
import Test.Hspec.Monadic (hspecX)

main :: IO ()
main = do
  runConn setup
  hspecX $ specs >> renameSpecs
