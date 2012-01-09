import PersistentTest
import RenameTest
import DataTypeTest
import Test.Hspec.Monadic (hspecX)

main :: IO ()
main = do
  runConn setup
  hspecX $ specs >> renameSpecs >> dataTypeSpecs
