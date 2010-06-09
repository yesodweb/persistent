{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

import Yesod hiding (Form)
import Database.Persist
import Database.Persist.Sqlite
import Data.Time (Day)
import Yesod.Helpers.Crud
import Data.Monoid (mempty)

share2 persistSqlite deriveFormable [$persist|
Entry
    slug Slug
    date Day Desc
    title NonEmptyString
    content Html
    UniqueSlug slug
    deriving
|]

data Blog = Blog { blogConn :: Database }

instance YesodPersist Blog where
    type YesodDB Blog = SqliteReader
    runDB x = getYesod >>= runSqlite x . blogConn

type BlogCrud = Crud Blog Entry

instance Item Entry where
    itemTitle = unNonEmptyString . entryTitle

mkYesod "Blog" [$parseRoutes|
/                         RootR              GET
/entry/#Slug              EntryR             GET
/admin                    AdminR             BlogCrud siteCrud defaultCrud
|]

instance Yesod Blog where approot _ = "http://localhost:3000"

getRootR :: Handler Blog RepHtml
getRootR = do
    entries <- fmap (map snd) $ runDB $ select [] [EntryDateDesc]
    applyLayout "Persistent Blog" mempty [$hamlet|
%h1 Welcome to the persistent blog.
%ul
    $forall entries entry
        %li
            %a!href=@EntryR.entrySlug.entry@
                $string.unNonEmptyString.entryTitle.entry$
%p
    %a!href=@AdminR.CrudListR@ Admin
|]

getEntryR :: Slug -> Handler Blog RepHtml
getEntryR slug = do
    (_, entry) <- runDB (getBy $ UniqueSlug slug) >>= maybe notFound return
    applyLayout (unNonEmptyString $ entryTitle entry) mempty [$hamlet|
%p
    %a!href=@RootR@ Return to homepage
%h1 $string.unNonEmptyString.entryTitle.entry$
%h2 $string.show.entryDate.entry$
#content $entryContent.entry$
|]

main :: IO ()
main = withSqlite "blog.db3" $ \conn -> do
    runSqlite (initialize (halfDefined :: Entry)) conn
    toWaiApp (Blog conn) >>= basicHandler 3000
