{-# language ScopedTypeVariables, DataKinds #-}

module RawSqlTest where

import Data.Coerce
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T

import Init
import PersistTestPetType
import PersistentTestModels

specsWith :: Runner SqlBackend m => RunDb SqlBackend m -> Spec
specsWith runDb = describe "rawSql" $ do
    it "2+2" $ runDb $ do
        ret <- rawSql "SELECT 2+2" []
        liftIO $ ret @?= [Single (4::Int)]

    it "?-?" $ runDb $ do
        ret <- rawSql "SELECT ?-?" [PersistInt64 5, PersistInt64 3]
        liftIO $ ret @?= [Single (2::Int)]

    it "NULL" $ runDb $ do
        ret <- rawSql "SELECT NULL" []
        liftIO $ ret @?= [Nothing :: Maybe (Single Int)]

    it "entity" $ runDb $ do
        Entity p1k p1 <- insertEntity $ Person "Mathias"   23 Nothing
        Entity p2k p2 <- insertEntity $ Person "Norbert"   44 Nothing
        Entity p3k _  <- insertEntity $ Person "Cassandra" 19 Nothing
        Entity _   _  <- insertEntity $ Person "Thiago"    19 Nothing
        Entity a1k a1 <- insertEntity $ Pet p1k "Rodolfo" Cat
        Entity a2k a2 <- insertEntity $ Pet p1k "Zeno"    Cat
        Entity a3k a3 <- insertEntity $ Pet p2k "Lhama"   Dog
        Entity _   _  <- insertEntity $ Pet p3k "Abacate" Cat
        escape <- getEscape
        person <- getTableName (error "rawSql Person" :: Person)
        name   <- getFieldName PersonName
        let query = T.concat [ "SELECT ??, ?? "
                             , "FROM ", person
                             , ", ", escape "Pet"
                             , " WHERE ", person, ".", escape "age", " >= ? "
                             , "AND ", escape "Pet", ".", escape "ownerId", " = "
                                     , person, ".", escape "id"
                             , " ORDER BY ", person, ".", name
                             ]
        ret <- rawSql query [PersistInt64 20]
        liftIO $ ret @?= [ (Entity p1k p1, Entity a1k a1)
                         , (Entity p1k p1, Entity a2k a2)
                         , (Entity p2k p2, Entity a3k a3) ]
        ret2 <- rawSql query [PersistInt64 20]
        liftIO $ ret2 @?= [ (Just (Entity p1k p1), Just (Entity a1k a1))
                          , (Just (Entity p1k p1), Just (Entity a2k a2))
                          , (Just (Entity p2k p2), Just (Entity a3k a3)) ]
        ret3 <- rawSql query [PersistInt64 20]
        liftIO $ ret3 @?= [ Just (Entity p1k p1, Entity a1k a1)
                          , Just (Entity p1k p1, Entity a2k a2)
                          , Just (Entity p2k p2, Entity a3k a3) ]

    it "order-proof" $ runDb $ do
        let p1 = Person "Zacarias" 93 Nothing
        p1k <- insert p1
        escape <- getEscape
        let query = T.concat [ "SELECT ?? "
                             , "FROM ", escape "Person"
                             ]
        ret1 <- rawSql query []
        ret2 <- rawSql query [] :: MonadIO m => SqlPersistT m [Entity (ReverseFieldOrder Person)]
        liftIO $ ret1 @?= [Entity p1k p1]
        liftIO $ ret2 @?= [Entity (RFOKey $ unPersonKey p1k) (RFO p1)]

    it "permits prefixes" $ runDb $ do
        let r1 = Relationship "Foo" Nothing
        r1k <- insert r1
        let r2 = Relationship "Bar" (Just r1k)
        r2k <- insert r2
        let r3 = Relationship "Lmao" (Just r1k)
        r3k <- insert r3
        let r4 = Relationship "Boring" (Just r2k)
        r4k <- insert r4
        escape <- getEscape
        let query = T.concat
                [ "SELECT ??, ?? "
                , "FROM ", escape "Relationship", " AS parent "
                , "LEFT OUTER JOIN ", escape "Relationship", " AS child "
                , "ON parent.id = child.parent"
                ]

        result :: [(EntityWithPrefix "parent" Relationship, Maybe (EntityWithPrefix "child" Relationship))] <-
            rawSql query []

        liftIO $
            coerce result `shouldMatchList`
                [ (Entity r1k r1, Just (Entity r2k r2))
                , (Entity r1k r1, Just (Entity r3k r3))
                , (Entity r2k r2, Just (Entity r4k r4))
                , (Entity r3k r3, Nothing)
                , (Entity r4k r4, Nothing)
                ]


    it "OUTER JOIN" $ runDb $ do
        let insert' :: (PersistStore backend, PersistEntity val, PersistEntityBackend val ~ BaseBackend backend, MonadIO m)
                    => val -> ReaderT backend m (Key val, val)
            insert' v = insert v >>= \k -> return (k, v)
        (p1k, p1) <- insert' $ Person "Mathias"   23 Nothing
        (p2k, p2) <- insert' $ Person "Norbert"   44 Nothing
        (a1k, a1) <- insert' $ Pet p1k "Rodolfo" Cat
        (a2k, a2) <- insert' $ Pet p1k "Zeno"    Cat
        escape <- getEscape
        let query = T.concat [ "SELECT ??, ?? "
                             , "FROM ", person
                             , "LEFT OUTER JOIN ", pet
                             , " ON ", person, ".", escape "id"
                             , " = ", pet, ".", escape "ownerId"
                             , " ORDER BY ", person, ".", escape "name"]
            person = escape "Person"
            pet    = escape "Pet"
        ret <- rawSql query []
        liftIO $ ret @?= [ (Entity p1k p1, Just (Entity a1k a1))
                         , (Entity p1k p1, Just (Entity a2k a2))
                         , (Entity p2k p2, Nothing) ]

    it "handles lower casing" $
        runDb $ do
            C.runConduitRes $ rawQuery "SELECT full_name from lower_case_table WHERE my_id=5" [] C..| CL.sinkNull
            C.runConduitRes $ rawQuery "SELECT something_else from ref_table WHERE id=4" [] C..| CL.sinkNull

    it "commit/rollback" $ do
        caseCommitRollback runDb
        runDb cleanDB

    it "queries with large number of results" $ runDb $ do
        -- max size of a GHC tuple is 62, but Eq instances currently only exist up to 15-tuples
        -- See https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3369
        ret <- rawSql "SELECT ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?" $ map PersistInt64 [1..15]
        liftIO $ ret @?= [(Single (1::Int), Single (2::Int), Single (3::Int), Single (4::Int), Single (5::Int), Single (6::Int), Single (7::Int), Single (8::Int), Single (9::Int), Single (10::Int), Single (11::Int), Single (12::Int), Single (13::Int), Single (14::Int), Single (15::Int))]

getEscape :: MonadReader SqlBackend m => m (Text -> Text)
getEscape = asks ((. DBName) . connEscapeName)

caseCommitRollback :: Runner SqlBackend m => RunDb SqlBackend m -> Assertion
caseCommitRollback runDb = runDb $ do
    let filt :: [Filter Person1]
        filt = []

    let p = Person1 "foo" 0

    _ <- insert p
    _ <- insert p
    _ <- insert p

    c1 <- count filt
    c1 @== 3

    transactionSave
    c2 <- count filt
    c2 @== 3

    _ <- insert p
    transactionUndo
    c3 <- count filt
    c3 @== 3

    _ <- insert p
    transactionSave
    _ <- insert p
    _ <- insert p
    transactionUndo
    c4 <- count filt
    c4 @== 4
