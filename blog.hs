{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Yesod hiding (Form)
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.Sqlite
import Data.Time (Day)
import Safe
import Control.Applicative
import Control.Applicative.Error
import Control.Arrow (second)
import Yesod.Contrib.Formable
import Text.Formlets
import Text.Hamlet.Monad (hamletToText)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Char

newtype Slug = Slug { unSlug :: String }
    deriving (Read, Eq, Show, SinglePiece, Persistable)

instance Fieldable Slug where
    fieldable label x = input' go (fmap unSlug x) `check` asSlug
      where
        go name val = [$hamlet|
%tr
    %th $pack'.label$
    %td
        %input!type=text!name=$pack'.name$!value=$pack'.val$
|]
        asSlug [] = Failure ["Slug must be non-empty"]
        asSlug x
            | all (\c -> c `elem` "-_" || isAlphaNum c) x =
                Success $ Slug x
            | otherwise = Failure ["Slug must be alphanumeric, - and _"]

persistSqlite [$persist|
Entry
    slug Slug
    date Day Desc
    title String
    content HtmlContent
    UniqueSlug slug
|]

deriving instance Show Entry

instance Formable Entry where
    formable Nothing =
        Entry <$> fieldable "Slug" Nothing
              <*> fieldable "Date" Nothing
              <*> fieldable "Title" Nothing
              <*> fieldable "Content" Nothing
    formable (Just (Entry a b c d)) =
        Entry <$> fieldable "Slug" (Just a)
              <*> fieldable "Date" (Just b)
              <*> fieldable "Title" (Just c)
              <*> fieldable "Content" (Just d)

data Blog = Blog { conn :: Database }

runDB :: SqliteReader (Handler Blog) a -> Handler Blog a
runDB x = getYesod >>= runSqlite x . conn

mkYesod "Blog" [$parseRoutes|
/                         RootR              GET
/entry/#Slug              EntryR             GET
/entry/crud/add           AddEntryR          GET POST
/entry/crud/#Slug/edit    EditEntryR         GET POST
/entry/crud/#Slug/delete  DeleteEntryR       GET POST
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

getEntryR :: Slug -> Handler Blog RepHtml
getEntryR slug = do
    (_, entry) <- runDB (getBy $ UniqueSlug slug) >>= maybe notFound return
    applyLayout (entryTitle entry) (return ()) [$hamlet|
%p
    %a!href=@RootR@ Return to homepage
%h1 $cs.entryTitle.entry$
%h2 $cs.show.entryDate.entry$
#content $entryContent.entry$
|]

runForm :: Form xml (Handler y) a -> Handler y (Failing a, xml)
runForm f = do
    req <- getRequest
    (pp, _) <- liftIO $ reqRequestBody req
    let env = map (second Left) pp
    let (a, b, c) = runFormState env f
    a' <- a
    return (a', b)

helper :: String -> Bool -> Maybe (EntryId, Entry) -> Handler Blog RepHtml
helper title isPost me = do
    (errs, form) <- runForm $ formable $ fmap snd me
    errs' <- case (isPost, errs) of
                (True, Success a) -> do
                    case me of
                        Just (eid, _) -> runDB $ replace eid a
                        Nothing -> runDB $ insert a >> return ()
                    redirect RedirectTemporary $ EntryR $ entrySlug a
                (True, Failure e) -> return $ Just e
                (False, _) -> return Nothing
    applyLayout title (return ()) [$hamlet|
%h1 $cs.title$
$maybe errs' es
    %ul
        $forall es e
            %li $cs.e$
%form!method=post
    %table
        ^form^
        %tr
            %td!colspan=2
                %input!type=submit
|]

getAddEntryR :: Handler Blog RepHtml
getAddEntryR = helper "Add new entry" False Nothing

postAddEntryR :: Handler Blog RepHtml
postAddEntryR = helper "Add new entry" True Nothing

getEditEntryR :: Slug -> Handler Blog RepHtml
getEditEntryR slug = do
    e <- runDB (getBy $ UniqueSlug slug) >>= maybe notFound return
    helper "Edit entry" False $ Just e

postEditEntryR :: Slug -> Handler Blog RepHtml
postEditEntryR slug = do
    e <- runDB (getBy $ UniqueSlug slug) >>= maybe notFound return
    helper "Edit entry" True $ Just e

getDeleteEntryR :: Slug -> Handler Blog RepHtml
getDeleteEntryR slug = do
    _ <- runDB (getBy $ UniqueSlug slug) >>= maybe notFound return
    applyLayout "Confirm delete" (return ()) [$hamlet|
%form!method=post!action=@DeleteEntryR.slug@
    %h1 Really delete?
    %p
        %input!type=submit!value=Yes
        \ 
        %a!href=@RootR@ No
|]

postDeleteEntryR :: Slug -> Handler Blog RepHtml
postDeleteEntryR slug = do
    (eid, _) <- runDB (getBy $ UniqueSlug slug) >>= maybe notFound return
    runDB $ delete eid
    redirect RedirectTemporary RootR

main :: IO ()
main = withSqlite "blog.db3" $ \conn -> do
    runSqlite (initialize (halfDefined :: Entry)) conn
    toWaiApp (Blog conn) >>= basicHandler 3000
