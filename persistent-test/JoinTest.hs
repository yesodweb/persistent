{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module JoinTest ( specs
#ifndef WITH_MONGODB
, joinMigrate
#endif
) where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit (Assertion)

import Database.Persist
import Database.Persist.Query.Join (selectOneMany, SelectOneMany(..))
import qualified Database.Persist.Query.Join
import Database.Persist.TH (persistUpperCase)
import Control.Monad.IO.Class (MonadIO)

#ifndef WITH_MONGODB
import qualified Database.Persist.Query.Join.Sql
#endif

import Init

#ifdef WITH_MONGODB
mkPersist persistSettings [persistUpperCase|
#else
share [mkPersist sqlSettings,  mkMigrate "joinMigrate"] [persistUpperCase|
#endif

  Author
    name String
    deriving Show Eq
  Entry
    authorId AuthorId
    title String
    deriving Show Eq
|]
#ifdef WITH_MONGODB
cleanDB :: (PersistQuery m, PersistEntityBackend Entry ~ PersistMonadBackend m) => m ()
cleanDB = do
  deleteWhere ([] :: [Filter Author])
  deleteWhere ([] :: [Filter Entry])
db :: Action IO () -> Assertion
db = db' cleanDB
#endif


specs :: Spec
specs = describe "joins" $ do
  it "NoSql" $ db $ joinGeneric Database.Persist.Query.Join.runJoin False
#ifndef WITH_MONGODB
  it "Sql" $ db $ joinGeneric Database.Persist.Query.Join.Sql.runJoin True
#endif


joinGeneric :: (MonadIO m, PersistQuery m, backend ~ PersistMonadBackend m) =>
               (SelectOneMany (AuthorGeneric backend) (EntryGeneric backend)
                -> m [(Entity (AuthorGeneric backend), [Entity (EntryGeneric backend)])])
                -> Bool
                -> m ()

joinGeneric run _ = do
    a <- insert $ Author "a"
    a1 <- insert $ Entry a "a1"
    a2 <- insert $ Entry a "a2"
    a3 <- insert $ Entry a "a3"
    b <- insert $ Author "b"
    b1 <- insert $ Entry b "b1"
    b2 <- insert $ Entry b "b2"
    c <- insert $ Author "c"

    x <- run $ selectOneMany (EntryAuthorId <-.) entryAuthorId
    liftIO $
        x @?=
            [ ((Entity a $ Author "a"),
                [ (Entity a1 $ Entry a "a1")
                , (Entity a2 $ Entry a "a2")
                , (Entity a3 $ Entry a "a3")
                ])
            , ((Entity b $ Author "b"),
                [ (Entity b1 $ Entry b "b1")
                , (Entity b2 $ Entry b "b2")
                ])
            ]

    y <- run $ (selectOneMany (EntryAuthorId <-.) entryAuthorId)
            { somFilterOne = [AuthorName ==. "a"]
            }
    liftIO $
        y @?=
            [ ((Entity a $ Author "a"),
                [ (Entity a1 $ Entry a "a1")
                , (Entity a2 $ Entry a "a2")
                , (Entity a3 $ Entry a "a3")
                ])
            ]

    z <- run (selectOneMany (EntryAuthorId <-.) entryAuthorId)
            { somOrderOne = [Asc AuthorName]
            , somOrderMany = [Desc EntryTitle]
            }
    liftIO $
        z @?=
            [ ((Entity a $ Author "a"),
                [ (Entity a3 $ Entry a "a3")
                , (Entity a2 $ Entry a "a2")
                , (Entity a1 $ Entry a "a1")
                ])
            , ((Entity b $ Author "b"),
                [ (Entity b2 $ Entry b "b2")
                , (Entity b1 $ Entry b "b1")
                ])
            ]

    w <- run (selectOneMany (EntryAuthorId <-.) entryAuthorId)
            { somOrderOne = [Asc AuthorName]
            , somOrderMany = [Desc EntryTitle]
            , somIncludeNoMatch = True
            }
    liftIO $
        w @==
            [ ((Entity a $ Author "a"),
                [ (Entity a3 $ Entry a "a3")
                , (Entity a2 $ Entry a "a2")
                , (Entity a1 $ Entry a "a1")
                ])
            , ((Entity b $ Author "b"),
                [ (Entity b2 $ Entry b "b2")
                , (Entity b1 $ Entry b "b1")
                ])
            , ((Entity c $ Author "c"), [])
            ]


    wNull <- run (selectOneMany (EntryAuthorId <-.) entryAuthorId)
            { somOrderOne = [Asc AuthorName]
            , somOrderMany = [Desc EntryTitle]
            , somFilterMany = [EntryTitle ==. "this should not match anything"]
            , somIncludeNoMatch = True
            }
    liftIO $
        wNull @== [ ((Entity a $ Author "a"), [])
                  , ((Entity b $ Author "b"), [])
                  , ((Entity c $ Author "c"), [])
                  ]
