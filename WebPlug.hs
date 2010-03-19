module WebPlug where

import Network.Wai
import Network.Wai.Enumerator
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

newtype AbsPath = AbsPath { unAbsPath :: String }
newtype PathInfo = PathInfo { unPathInfo :: String }
handleWai :: (PathInfo -> Either err url)
          -> (err -> Application)
          -> (url -> PathInfo)
          -> (PathInfo -> AbsPath)
          -> (url -> (url -> AbsPath) -> Application)
          -> Application
handleWai parsePI onErr buildPI buildAbsPath dispatch req = do
    let pi = PathInfo $ S.unpack $ pathInfo req
    case parsePI pi of
        Right url -> dispatch url (buildAbsPath . buildPI) req
        Left err -> onErr err req

default404 :: Application
default404 _ = return $ Response
                Status404
                [(ContentType, S.pack "text/plain")]
                $ Right $ fromLBS $ L.pack "Not found"
