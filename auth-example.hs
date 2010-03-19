import Data.Persist
import WebPlug
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Network.Wai
import Network.Wai.Enumerator
import Network.Wai.Handler.SimpleServer
import Control.Arrow
import Web.Encodings
import Data.Maybe
import Debug.Trace

data User = User String String
    deriving Show

userTable = Table
    { tableName = "auth"
    , tableFields =
        [ Field "username" FTString False
        , Field "password" FTString False
        ]
    , tableFreeze = \(User u p) ->
        [ ("username", FVString u)
        , ("password", FVString p)
        ]
    , tableThaw = \x ->
        case x of
            [("username", FVString u), ("password", FVString p)] ->
                Just $ User u p
            _ -> Nothing
    }

data AuthRoutes = ARLogin
                | ARLogout
                | ARCheck

parseAuthRoutes :: PathInfo -> Either () AuthRoutes
parseAuthRoutes (PathInfo "/login/") = Right ARLogin
parseAuthRoutes (PathInfo "/logout/") = Right ARLogout
parseAuthRoutes (PathInfo "/check/") = Right ARCheck
parseAuthRoutes _ = Left ()

renderAuthRoutes :: AuthRoutes -> PathInfo
renderAuthRoutes ARLogin = PathInfo "/login/"
renderAuthRoutes ARLogout = PathInfo "/logout/"
renderAuthRoutes ARCheck = PathInfo "/check/"

htmlResponse :: String -> IO Response
htmlResponse s = return $ Response
    Status200
    [(ContentType, S.pack "text/html")]
    $ Right $ fromLBS $ L.pack s

checkLoggedIn :: Request -> Maybe String
checkLoggedIn req = do
    cookie <- lookup Cookie $ requestHeaders req
    let cookies = parseCookies $ S.unpack cookie
    lookup "AUTH" cookies

dispatchAuthRoutes :: DataStore s
                   => s
                   -> AuthRoutes
                   -> (AuthRoutes -> AbsPath)
                   -> Application
dispatchAuthRoutes _ ARCheck toAP req = htmlResponse $ concat
    [ "<p>You are currently "
    , if isLoggedIn then "" else "not "
    , "logged in</p>"
    , if isLoggedIn
        then ("<a href=\"" ++ unAbsPath (toAP ARLogout) ++ "\">Logout</a>")
        else ("<a href=\"" ++ unAbsPath (toAP ARLogin) ++ "\">Login</a>")
    ]
      where
        isLoggedIn = isJust $ checkLoggedIn req
dispatchAuthRoutes _ ARLogout toAP req = return $ Response
    Status200
    [ (SetCookie, S.pack
        "AUTH=; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT")
    , (ContentType, S.pack "text/html")
    ]
    $ Right $ fromLBS $ L.pack $ concat
        [ "<p>You are logged out.</p>"
        , "<a href=\"" ++ unAbsPath (toAP ARLogin) ++ "\">Login</a>"
        ]
dispatchAuthRoutes ds ARLogin toAP req
    | requestMethod req == GET = htmlResponse $ concat
        [ "<form method=\"POST\" action=\""
        , unAbsPath $ toAP ARLogin
        , "\"><table><tr><th>Username</th>"
        , "<td><input type=\"text\" name=\"username\"></td></tr><tr>"
        , "<th>Password</th>"
        , "<td><input type=\"password\" name=\"password\"></td></tr>"
        , "<tr><td colspan=2><input type=\"submit\"></td></tr>"
        , "</table></form>"
        ]
    | requestMethod req == POST = do
        (posts', _) <- parseRequestBody lbsSink req
        let posts = map (L.unpack *** L.unpack) posts'
        let u = fromMaybe "" $ lookup "username" posts
        let p = fromMaybe "" $ lookup "password" posts
        res <- filterTable ds userTable
                [ Filter "username" (FVString u) [EQ]
                , Filter "password" (FVString p) [EQ]
                ]
        case res of
            [] -> htmlResponse $ concat
                [ "<p>Login failed.</p>"
                , "<a href=\""
                , unAbsPath $ toAP ARLogin
                , "\">Try again</a>"
                ]
            ((rid, _):_) -> return $ Response Status200
                [ (ContentType, S.pack "text/html")
                , (SetCookie, S.pack $ concat
                    [ "AUTH="
                    , show rid
                    , "; path=/; expires=Wed, 01-Jan-2020 00:00:00 GMT"
                    ])
                ]
                $ Right $ fromLBS $ L.pack "<h1>Logged in</h1>"
    | otherwise = default404 req

authPlug :: DataStore ds => ds -> WebPlug
authPlug ds = WebPlug
    { wpParseUrl = parseAuthRoutes
    , wpRenderUrl = renderAuthRoutes
    , wpDispatch = dispatchAuthRoutes ds
    }

data MySite = MyHome | MyAuth AuthRoutes

parseMyRoutes (PathInfo "/") = Right MyHome
parseMyRoutes (PathInfo ('/':'a':'u':'t':'h':'/':rest)) =
    either Left (Right . MyAuth) $ parseAuthRoutes $ PathInfo $ '/' : rest
renderMyRoutes MyHome = PathInfo "/"
renderMyRoutes (MyAuth auth) =
    PathInfo $ "/auth" ++ unPathInfo (renderAuthRoutes auth)

dispatchMyRoutes :: DataStore s
                 => s
                 -> MySite
                 -> (MySite -> AbsPath)
                 -> Application
dispatchMyRoutes _ MyHome toAP req = htmlResponse $ concat
    [ "<p>Welcome to my homepage!</p>"
    , "<p>Current auth: "
    , show $ checkLoggedIn req
    , "</p>"
    , "<p><a href=\""
    , unAbsPath $ toAP $ MyAuth ARLogin
    , "\">Login</a> <a href=\""
    , unAbsPath $ toAP $ MyAuth ARLogout
    , "\">Logout</a> <a href=\""
    , unAbsPath $ toAP $ MyAuth ARCheck
    , "\">Check</a></p>"
    ]
dispatchMyRoutes ds (MyAuth auth) toAP req =
    dispatchAuthRoutes ds auth (toAP . MyAuth) req

main = do
    ms <- createMemoryStore
    initTable ms userTable
    createRecord ms userTable $ User "michael" "michael"
    putStrLn "Running, connect to http://localhost:3000/"
    run 3000 $ handleWai
        parseMyRoutes
        (const default404)
        renderMyRoutes
        (AbsPath . ((++) "http://localhost:3000") . unPathInfo)
        (dispatchMyRoutes ms)
