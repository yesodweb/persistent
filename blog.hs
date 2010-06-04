{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

import Yesod hiding (Form)
import Database.Persist
import Database.Persist.Sqlite
import Data.Time (Day)
import Yesod.Contrib

share2 persistSqlite deriveFormable [$persist|
Entry
    slug Slug
    date Day Desc
    title String
    content HtmlContent
    UniqueSlug slug
|]

data Blog = Blog { blogConn :: Database }

instance YesodPersist Blog where
    type YesodDB Blog = SqliteReader
    runDB x = getYesod >>= runSqlite x . blogConn

mkYesod "Blog" [$parseRoutes|
/                         RootR              GET
/entry/#Slug              EntryR             GET
/entry/crud/add           AddEntryR          GET POST
/entry/crud/#Slug/edit    EditEntryR         GET POST
/entry/crud/#Slug/delete  DeleteEntryR       GET POST
|]

instance Crudable Entry where
    type CrudApp Entry = Blog
    crudList _ = RootR
    crudCreate _ = AddEntryR
    crudRead = EntryR . entrySlug
    crudEdit = EditEntryR . entrySlug
    crudDelete = DeleteEntryR . entrySlug

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

getAddEntryR :: Handler Blog RepHtml
getAddEntryR = crudHelper "Add new entry" False (Nothing :: Maybe (EntryId, Entry))

postAddEntryR :: Handler Blog RepHtml
postAddEntryR = crudHelper "Add new entry" True (Nothing :: Maybe (EntryId, Entry))

getEditEntryR :: Slug -> Handler Blog RepHtml
getEditEntryR slug = do
    e <- runDB (getBy $ UniqueSlug slug) >>= maybe notFound return
    crudHelper "Edit entry" False $ Just e

postEditEntryR :: Slug -> Handler Blog RepHtml
postEditEntryR slug = do
    e <- runDB (getBy $ UniqueSlug slug) >>= maybe notFound return
    crudHelper "Edit entry" True $ Just e

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
