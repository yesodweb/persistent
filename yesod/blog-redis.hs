{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Yesod
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Redis
import Database.Redis.Redis (connect, localhost, defaultPort, Redis)
import Data.Time (Day)
import Data.Convertible (Convertible (..))
import Data.Convertible.Text
import Database.HDBC (SqlValue (..))
import Data.ByteString (ByteString)
import Safe
import Control.Applicative
import Data.Maybe

instance ConvertAttempt String HtmlContent where
    convertAttempt = return . Encoded . cs
instance ConvertSuccess HtmlContent String where
    convertSuccess = cs . htmlContentToText
instance Convertible HtmlContent SqlValue where
    safeConvert = Right . SqlByteString . cs . htmlContentToText
instance Convertible SqlValue HtmlContent where
    safeConvert x =
        case safeConvert x of
            Left e -> Left e
            Right bs -> Right $ Encoded $ cs (bs :: ByteString)

persistRedis [$persist|
Entry
    slug String
    date Day Desc
    title String
    content HtmlContent
    UniqueSlug slug
|]

data Blog = Blog { conn :: Redis }

runDB :: RedisReader (Handler Blog) a -> Handler Blog a
runDB x = getYesod >>= runRedis x . conn

mkYesod "Blog" [$parseRoutes|
/                           RootR              GET
/entry/#String              EntryR             GET
/entry/crud/add             AddEntryR          GET POST
/entry/crud/#String/edit    EditEntryR         GET POST
/entry/crud/#String/delete  DeleteEntryR       GET POST
|]

instance Yesod Blog where approot _ = "http://localhost:3000"

getRootR :: Handler Blog RepHtml
getRootR = do
    entries <- fmap (map snd) $ runDB $ select [] [EntryDateDesc]
    applyLayout "Persistent Blog" (return ()) [$hamlet|
%h1 Welcome to the persistent blog.
%ul
    $forall entries entry
        %li
            %a!href=@EntryR.entrySlug.entry@ $cs.entryTitle.entry$
            \ (
            %a!href=@EditEntryR.entrySlug.entry@ edit
            \ 
            %a!href=@DeleteEntryR.entrySlug.entry@ delete
            )
%p
    %a!href=@AddEntryR@ Add new entry
|]

getBy' (UniqueSlug slug) = do
    a <- select [] []
    return $ listToMaybe $ filter (\(_, x) -> entrySlug x == slug) a

getEntryR :: String -> Handler Blog RepHtml
getEntryR slug = do
    (_, entry) <- runDB (getBy' $ UniqueSlug slug) >>= maybe notFound return
    applyLayout (entryTitle entry) (return ()) [$hamlet|
%p
    %a!href=@RootR@ Return to homepage
%h1 $cs.entryTitle.entry$
%h2 $cs.show.entryDate.entry$
#content $entryContent.entry$
|]

class HasForm a where
    getForm :: Monad m => Maybe a -> Hamlet url m ()
    parseForm :: [(String, String)]
              -> Either (Hamlet url IO ()) a

instance HasForm Entry where
    getForm x = [$hamlet|
%table
    %tr
        %th Slug
        %td
            %input!type=text!name=slug!value=$cs.slug$
    %tr
        %th Title
        %td
            %input!type=text!name=title!value=$cs.title$
    %tr
        %th Date
        %td
            %input!type=date!name=date!value=$cs.date$
    %tr
        %th Content
        %td
            %textarea!name=content
                $content$
    %tr
        %td!colspan=2
            %input!type=submit
|]
      where
        slug = maybe "" entrySlug x
        title = maybe "" entryTitle x
        date = maybe "" (show . entryDate) x
        content = maybe (cs "") (Unencoded . htmlContentToText . entryContent) x
    parseForm params =
        let params' = filter (not . null . snd) params
            slug = lookup "slug" params'
            title = lookup "title" params'
            date = lookup "date" params' >>= readMay
            content = fmap (Encoded . cs) $ lookup "content" params'
         in case Entry <$> slug <*> date <*> title <*> content of
                Just e -> Right e
                Nothing -> Left $ getForm (Nothing :: Maybe Entry) -- FIXME

getAddEntryR :: Handler Blog RepHtml
getAddEntryR = do
    let form = getForm (Nothing :: Maybe Entry)
    mmsg <- getMessage
    applyLayout "Add new entry" (return ()) [$hamlet|
%h1 Add new entry
$maybe mmsg msg
    %p.message $msg$
%form!method=post!action=@AddEntryR@
    ^form^
|]

postAddEntryR :: Handler Blog RepHtml
postAddEntryR = do
    req <- getRequest
    (pp, _) <- liftIO $ reqRequestBody req
    case parseForm pp of
        Left _ -> do
            setMessage $ cs "Errors in your submission"
            redirect RedirectTemporary AddEntryR
        Right e -> do
            runDB $ insert e
            redirect RedirectTemporary $ EntryR $ entrySlug e

getEditEntryR :: String -> Handler Blog RepHtml
getEditEntryR slug = do
    (_, entry) <- runDB (getBy' $ UniqueSlug slug) >>= maybe notFound return
    let form = getForm $ Just entry
    mmsg <- getMessage
    applyLayout "Edit entry" (return ()) [$hamlet|
%h1 Edit entry
$maybe mmsg msg
    %p.message $msg$
%form!method=post!action=@EditEntryR.slug@
    ^form^
|]

postEditEntryR :: String -> Handler Blog RepHtml
postEditEntryR slug = do
    req <- getRequest
    (eid, _) <- runDB (getBy' $ UniqueSlug slug) >>= maybe notFound return
    (pp, _) <- liftIO $ reqRequestBody req
    case parseForm pp of
        Left _ -> do
            setMessage $ cs "Errors in your submission"
            redirect RedirectTemporary $ EditEntryR slug
        Right e -> do
            runDB $ replace eid e
            redirect RedirectTemporary $ EntryR $ entrySlug e

getDeleteEntryR :: String -> Handler Blog RepHtml
getDeleteEntryR slug = do
    _ <- runDB (getBy' $ UniqueSlug slug) >>= maybe notFound return
    applyLayout "Confirm delete" (return ()) [$hamlet|
%form!method=post!action=@DeleteEntryR.slug@
    %h1 Really delete?
    %p
        %input!type=submit!value=Yes
        \ 
        %a!href=@RootR@ No
|]

postDeleteEntryR :: String -> Handler Blog RepHtml
postDeleteEntryR slug = do
    (eid, _) <- runDB (getBy' $ UniqueSlug slug) >>= maybe notFound return
    runDB $ delete eid
    redirect RedirectTemporary RootR

main :: IO ()
main = do
    conn <- connect localhost defaultPort
    runRedis (initialize (undefined :: Entry)) conn
    toWaiApp (Blog conn) >>= basicHandler 3000
