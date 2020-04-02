{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, UndecidableInstances #-}

module PersistentTest
    ( module PersistentTest
    , cleanDB
    , testMigrate
    , noPrefixMigrate
    , customPrefixMigrate
    , treeMigrate
    ) where

import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.Aeson
import Data.Conduit
import qualified Data.Text as T
import qualified Data.Conduit.List as CL
import Data.Functor.Constant
import Data.Functor.Identity
import qualified Data.HashMap.Lazy as M
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Test.Hspec.QuickCheck(prop)
import Test.HUnit hiding (Test)
import UnliftIO (MonadUnliftIO, catch)
import Web.PathPieces (PathPiece (..))
import Data.Proxy (Proxy(..))

import Database.Persist
import Database.Persist.Quasi
import Init
import PersistentTestModels
import PersistTestPetType
import PersistTestPetCollarType

catchPersistException :: (MonadUnliftIO m, MonadFail m) => m a -> b -> m b
catchPersistException action errValue = do
    Left res <-
      (Right `fmap` action) `catch`
      (\(_::PersistException) -> return $ Left errValue)
    return  res

filterOrSpecs
    :: forall m backend. Runner backend m
    => RunDb backend m
    -> Spec
filterOrSpecs runDb = describe "FilterOr" $ do
            it "FilterOr []" $ runDb $ do
                let p = Person "z" 1 Nothing
                _ <- insert p
                ps <- selectList [FilterOr []] [Desc PersonAge]
                assertEmpty ps
            it "||. []" $ runDb $ do
                let p = Person "z" 1 Nothing
                _ <- insert p
                c <- count $ [PersonName ==. "a"] ||. []
                c @== (1::Int)

-- Test proper polymorphism
_polymorphic :: (MonadFail m, MonadIO m, PersistQuery backend, BaseBackend backend ~ PersistEntityBackend Pet) => ReaderT backend m ()
_polymorphic = do
    ((Entity id' _):_) <- selectList [] [LimitTo 1]
    _ <- selectList [PetOwnerId ==. id'] []
    _ <- insert $ Pet id' "foo" Cat
    return ()

-- Some lens stuff
type ASetter s t a b = (a -> Identity b) -> s -> Identity t

set :: ASetter s t a b -> b -> s -> t
set l b = runIdentity . (l (\_ -> Identity b))

type Getting r s t a b = (a -> Constant r b) -> s -> Constant r t

view :: s -> Getting a s t a b -> a
view s l = getConstant (l Constant s)

specsWith :: forall backend m. Runner backend m => RunDb backend m -> Spec
specsWith runDb = describe "persistent" $ do
  it "fieldLens" $ do
      let michael = Entity undefined $ Person "Michael" 28 Nothing :: Entity Person
          michaelP1 = Person "Michael" 29 Nothing :: Person
      view michael (fieldLens PersonAge) @?= 28
      entityVal (set (fieldLens PersonAge) 29 michael) @?= michaelP1

  it "FilterAnd []" $ runDb $ do
      let p = Person "z" 1 Nothing
      _ <- insert p
      ps <- selectList [FilterAnd []] [Desc PersonAge]
      assertNotEmpty ps

  it "Filter In" $ runDb $ do
    _ <- selectList [Filter PersonName (FilterValues ["Kostas"]) In] []
    return ()

  it "order of opts is irrelevant" $ runDb $ do
      let eq (a, b, _) (c, d) = (a, b) @== (c, d)
          limitOffsetOrder' :: [SelectOpt Person] -> (Int, Int, [SelectOpt Person])
          limitOffsetOrder' = limitOffsetOrder
      limitOffsetOrder' [Desc PersonAge] `eq` (0, 0)
      limitOffsetOrder' [LimitTo 2, Desc PersonAge] `eq` (2, 0)
      limitOffsetOrder' [Desc PersonAge, LimitTo 2] `eq` (2, 0)
      limitOffsetOrder' [LimitTo 2, Desc PersonAge, OffsetBy 3] `eq` (2, 3)

      insertMany_ [ Person "z" 1 Nothing
                  , Person "y" 2 Nothing
                  , Person "x" 1 Nothing
                  , Person "w" 2 Nothing
                  , Person "v" 1 Nothing
                  , Person "u" 2 Nothing
                  ]

      a <- map (personName . entityVal) <$> selectList [] [Desc PersonAge, Asc PersonName, OffsetBy 2, LimitTo 3]
      a @== ["y", "v", "x"]

      b <- map (personName . entityVal) <$> selectList [] [OffsetBy 2, Desc PersonAge, LimitTo 3, Asc PersonName]
      b @== a

      c <- map (personName . entityVal) <$> selectList [] [OffsetBy 2, Desc PersonAge, LimitTo 3, Asc PersonName, LimitTo 1, OffsetBy 1]
      c @== a


  it "passes the general tests" $ runDb $ do
      let mic26 = Person "Michael" 26 Nothing
      micK <- insert mic26
      results <- selectList [PersonName ==. "Michael"] []
      results @== [Entity micK mic26]

      results' <- selectList [PersonAge <. 28] []
      results' @== [Entity micK mic26]

      p28 <- updateGet micK [PersonAge =. 28]
      personAge p28 @== 28

      updateWhere [PersonName ==. "Michael"] [PersonAge =. 29]
      uc <- count [PersonName ==. "Michael"]
      uc @== 1
      Just mic29 <- get micK
      personAge mic29 @== 29

      let eli = Person "Eliezer" 2 $ Just "blue"
      _ <- insert eli
      pasc <- selectList [] [Asc PersonAge]
      map entityVal pasc @== [eli, mic29]

      let abe30 = Person "Abe" 30 $ Just "black"
      _ <- insert abe30
      -- pdesc <- selectList [PersonAge <. 30] [Desc PersonName]
      map entityVal pasc @== [eli, mic29]

      abes <- selectList [PersonName ==. "Abe"] []
      map entityVal abes @== [abe30]

      Just (Entity _ p3) <- getBy $ PersonNameKey "Michael"
      p3 @== mic29

      ps <- selectList [PersonColor ==. Just "blue"] []
      map entityVal ps @== [eli]

      ps2 <- selectList [PersonColor ==. Nothing] []
      map entityVal ps2 @== [mic29]

      delete micK
      Nothing <- get micK
      return ()
  it "persistIdField" $ runDb $ do
      let p = Person "foo" 100 (Just "blue")
          q = Person "bar" 101 Nothing
      pk <- insert p
      qk <- insert q

      mp <- selectFirst [persistIdField ==. pk] []
      fmap entityVal mp @== Just p

      mq <- selectFirst [persistIdField ==. qk] []
      fmap entityVal mq @== Just q

  it "!=." $ runDb $ do
      deleteWhere ([] :: [Filter (PersonGeneric backend)])
      let mic = Person "Michael" 25 Nothing
      _ <- insert mic
      let eli = Person "Eliezer" 25 (Just "Red")
      _ <- insert eli

      pne <- selectList [PersonName !=. "Michael"] []
      map entityVal pne @== [eli]

      ps <- selectList [PersonColor !=. Nothing] []
      map entityVal ps @== [eli]

      pnm <- selectList [PersonName !=. "Eliezer"] []
      map entityVal pnm @== [mic]


  it "Double Maybe" $ runDb $ do
      deleteWhere ([] :: [Filter (PersonMayGeneric backend)])
      let mic = PersonMay (Just "Michael") Nothing
      _ <- insert mic
      let eli = PersonMay (Just "Eliezer") (Just "Red")
      _ <- insert eli
      pe <- selectList [PersonMayName ==. Nothing, PersonMayColor ==. Nothing] []
      map entityVal pe @== []
      pne <- selectList [PersonMayName !=. Nothing, PersonMayColor !=. Nothing] []
      map entityVal pne @== [eli]

  it "and/or" $ runDb $ do
      deleteWhere ([] :: [Filter (Person1Generic backend)])
      insertMany_ [ Person1 "Michael" 25
                  , Person1 "Miriam" 25
                  , Person1 "Michael" 30
                  , Person1 "Michael" 35
                  ]

      c10 <- count $ [Person1Name ==. "Michael"] ||. [Person1Name ==. "Miriam", Person1Age ==. 25]
      c10 @== 4
      c12 <- count [FilterOr [FilterAnd [Person1Name ==. "Michael"], FilterAnd [Person1Name ==. "Miriam"]]]
      c12 @== 4
      c14 <- count [FilterOr [FilterAnd [Person1Name ==. "Michael"], FilterAnd [Person1Name ==. "Miriam"],
                              FilterAnd [Person1Age >. 29, Person1Age <=. 30]]]
      c14 @== 4

      c20 <- count $ [Person1Name ==. "Miriam"] ||. [Person1Age >. 29, Person1Age <=. 30]
      c20 @== 2
      c22 <- count $ [Person1Age <=. 30] ++ [Person1Age >. 29]
      c22 @== 1
      c24 <- count $ [FilterAnd [Person1Age <=. 30, Person1Age >. 29]]
      c24 @== 1
      c26 <- count $ [Person1Age <=. 30] ++ [Person1Age >. 29]
      c26 @== 1

      c34 <- count $ [Person1Name ==. "Michael"] ||. [Person1Name ==. "Mirieam"] ++ [Person1Age <.35]
      c34 @== 3
      c30 <- count $ ([Person1Name ==. "Michael"] ||. [Person1Name ==. "Miriam"]) ++ [Person1Age <.35]
      c30 @== 3
      c36 <- count $ [Person1Name ==. "Michael"] ||. ([Person1Name ==. "Miriam"] ++ [Person1Age <.35])
      c36 @== 4

      c40 <- count $ ([Person1Name ==. "Michael"] ||. [Person1Name ==. "Miriam"] ||. [Person1Age <.35])
      c40 @== 4


  it "deleteWhere" $ runDb $ do
      key2 <- insert $ Person "Michael2" 90 Nothing
      _    <- insert $ Person "Michael3" 90 Nothing
      let p91 = Person "Michael4" 91 Nothing
      key91 <- insert p91

      ps90 <- selectList [PersonAge ==. 90] []
      assertNotEmpty ps90
      deleteWhere [PersonAge ==. 90]
      ps90' <- selectList [PersonAge ==. 90] []
      assertEmpty ps90'
      Nothing <- get key2

      Just p2_91 <- get key91
      p91 @== p2_91


  it "deleteBy" $ runDb $ do
      _ <- insert $ Person "Michael2" 27 Nothing
      let p3 = Person "Michael3" 27 Nothing
      key3 <- insert p3

      ps2 <- selectList [PersonName ==. "Michael2"] []
      assertNotEmpty ps2

      deleteBy $ PersonNameKey "Michael2"
      ps2' <- selectList [PersonName ==. "Michael2"] []
      assertEmpty ps2'

      Just p32 <- get key3
      p3 @== p32


  it "delete" $ runDb $ do
      key2 <- insert $ Person "Michael2" 27 Nothing
      let p3 = Person "Michael3" 27 Nothing
      key3 <- insert p3

      pm2 <- selectList [PersonName ==. "Michael2"] []
      assertNotEmpty pm2
      delete key2
      pm2' <- selectList [PersonName ==. "Michael2"] []
      assertEmpty pm2'

      Just p <- get key3
      p3 @== p

  prop "toPathPiece . fromPathPiece" $ \piece ->
      let key1 = piece :: (BackendKey SqlBackend)
          key2 = fromJust $ fromPathPiece $ toPathPiece key1 :: (BackendKey SqlBackend)
      in  toPathPiece key1 == toPathPiece key2

  it "replace" $ runDb $ do
      key2 <- insert $ Person "Michael2" 27 Nothing
      let p3 = Person "Michael3" 27 Nothing
      replace key2 p3
      Just p <- get key2
      p @== p3

      -- test replace an empty key
      delete key2
      Nothing <- get key2
      _ <- replace key2 p3
      Nothing <- get key2
      return ()

      let mic = Person "Michael" 25 Nothing
      micK <- insert mic
      Just p1 <- get micK
      p1 @== mic

      replace micK $ Person "Michael" 25 Nothing
      Just p2 <- get micK
      p2 @== mic

      replace micK $ Person "Michael" 26 Nothing
      Just mic26 <- get micK
      mic26 @/= mic
      personAge mic26 @== personAge mic + 1



  it "getBy" $ runDb $ do
      let p2 = Person "Michael2" 27 Nothing
      key2 <- insert p2
      Just (Entity k p) <- getBy $ PersonNameKey "Michael2"
      p @== p2
      k @== key2
      Nothing <- getBy $ PersonNameKey "Michael9"

      Just (Entity k' p') <- getByValue p2
      k' @== k
      p' @== p
      return ()


  it "updateGet" $ runDb $ do
      let p25 = Person "Michael" 25 Nothing
      key25 <- insert p25
      pBlue28 <- updateGet key25 [PersonAge =. 28, PersonName =. "Updated"]
      pBlue28 @== Person "Updated" 28 Nothing
      pBlue30 <- updateGet key25 [PersonAge +=. 2]
      pBlue30 @== Person "Updated" 30 Nothing

  describe "repsertMany" $ do
    it "adds new rows when no conflicts" $ runDb $ do
        ids@[johnId, janeId, aliceId, eveId] <- replicateM 4 $ liftIO (Person1Key `fmap` generateKey)
        let john = Person1 "john" 20
        let jane = Person1 "jane" 18
        let alice = Person1 "alice" 18
        let eve = Person1 "eve" 19

        insertKey johnId john
        insertKey janeId jane

        _ <- repsertMany [ (aliceId, alice), (eveId, eve) ]
        es <- getMany ids

        let rs = [john, jane, alice, eve]
        es @== Map.fromList (zip ids rs)
        mapM_ delete ids

    it "handles conflicts by replacing old keys with new records" $ runDb $ do
        let john = Person1 "john" 20
        let jane = Person1 "jane" 18
        let alice = Person1 "alice" 18
        let eve = Person1 "eve" 19

        johnId <- insert john
        janeId <- insert jane

        _ <- repsertMany [ (johnId, alice), (janeId, eve) ]
        (Just alice') <- get johnId
        (Just eve') <- get janeId

        [alice',eve'] @== [alice,eve]
        mapM_ delete [johnId, janeId]

  it "updateWhere" $ runDb $ do
      let p1 = Person "Michael" 25 Nothing
      let p2 = Person "Michael2" 25 Nothing
      key1 <- insert p1
      key2 <- insert p2
      updateWhere [PersonName ==. "Michael2"]
                  [PersonAge +=. 3, PersonName =. "Updated"]
      Just pBlue28 <- get key2
      pBlue28 @== Person "Updated" 28 Nothing
      Just p <- get key1
      p @== p1

  it "selectList" $ runDb $ do
      let p25 = Person "Michael" 25 Nothing
      let p26 = Person "Michael2" 26 Nothing
      [key25, key26] <- insertMany [p25, p26]
      ps1 <- selectList [] [Asc PersonAge]
      ps1 @== [(Entity key25 p25), (Entity key26 p26)]
      -- limit
      ps2 <- selectList [] [Asc PersonAge, LimitTo 1]
      ps2 @== [(Entity key25 p25)]
      -- offset
      ps3 <- selectList [] [Asc PersonAge, OffsetBy 1]
      ps3 @== [(Entity key26 p26)]
      -- limit & offset
      ps4 <- selectList [] [Asc PersonAge, LimitTo 1, OffsetBy 1]
      ps4 @== [(Entity key26 p26)]

      ps5 <- selectList [] [Desc PersonAge]
      ps5 @== [(Entity key26 p26), (Entity key25 p25)]
      ps6 <- selectList [PersonAge ==. 26] []
      ps6 @== [(Entity key26 p26)]

  it "selectSource" $ runDb $ do
      let p1 = Person "selectSource1" 1 Nothing
          p2 = Person "selectSource2" 2 Nothing
          p3 = Person "selectSource3" 3 Nothing
      [k1,k2,k3] <- insertMany [p1, p2, p3]

      ps1 <- runConduitRes $ selectSource [] [Desc PersonAge] .| await
      ps1 @== Just (Entity k3 p3)

      ps2 <- runConduitRes $ selectSource [PersonAge <. 3] [Asc PersonAge] .| CL.consume
      ps2 @== [Entity k1 p1, Entity k2 p2]

      runConduitRes $ selectSource [] [Desc PersonAge] .| do
          e1 <- await
          e1 @== Just (Entity k3 p3)

          e2 <- await
          e2 @== Just (Entity k2 p2)

          e3 <- await
          e3 @== Just (Entity k1 p1)

          e4 <- await
          e4 @== Nothing

  it "selectFirst" $ runDb $ do
      _ <- insert $ Person "Michael" 26 Nothing
      let pOld = Person "Oldie" 75 Nothing
      kOld <- insert pOld

      x <- selectFirst [] [Desc PersonAge]
      x @== Just (Entity kOld pOld)


  it "selectKeys" $ runDb $ do
      let p1 = Person "selectKeys1" 1 Nothing
          p2 = Person "selectKeys2" 2 Nothing
          p3 = Person "selectKeys3" 3 Nothing
      [k1,k2,k3] <- insertMany [p1, p2, p3]

      ps1 <- runConduitRes $ selectKeys [] [Desc PersonAge] .| await
      ps1 @== Just k3

      ps2 <- runConduitRes $ selectKeys [PersonAge <. 3] [Asc PersonAge] .| CL.consume
      ps2 @== [k1, k2]

      runConduitRes $ selectKeys [] [Desc PersonAge] .| do
          e1 <- await
          e1 @== Just k3

          e2 <- await
          e2 @== Just k2

          e3 <- await
          e3 @== Just k1

          e4 <- await
          e4 @== Nothing

  it "insertMany_ with no arguments" $ runDb $ do
    _ <- insertMany_ ([] :: [PersonGeneric backend])
    rows <- (count ([] :: [Filter (PersonGeneric backend)]) :: ReaderT backend m Int)
    rows @== 0
    _ <- insertMany ([] :: [PersonGeneric backend])
    rows2 <- count ([] :: [Filter (PersonGeneric backend)])
    rows2 @== 0
    _ <- insertEntityMany ([] :: [Entity (PersonGeneric backend)])
    rows3 <- count ([] :: [Filter (PersonGeneric backend)])
    rows3 @== 0

  it "insertEntityMany" $ runDb $ do
    id1:id2:id3:id4:id5:[] <- liftIO $ replicateM 5 (PersonKey `fmap` generateKey)
    let p1 = Entity id1 $ Person "insertEntityMany1" 1 Nothing
        p2 = Entity id2 $ Person "insertEntityMany2" 2 Nothing
        p3 = Entity id3 $ Person "insertEntityMany3" 3 Nothing
        p4 = Entity id4 $ Person "insertEntityMany4" 3 Nothing
        p5 = Entity id5 $ Person "insertEntityMany5" 3 Nothing
    insertEntityMany [p1,p2,p3,p4,p5]
    rows <- count ([] :: [Filter (PersonGeneric backend)])
    rows @== 5

  it "insertBy" $ runDb $ do
      Right _ <- insertBy $ Person "name" 1 Nothing
      Left _ <- insertBy $ Person "name" 1 Nothing
      Right _ <- insertBy $ Person "name2" 1 Nothing
      return ()

  it "insertKey" $ runDb $ do
      k <- liftIO (PersonKey `fmap` generateKey)
      insertKey k $ Person "Key" 26 Nothing
      Just (Entity k2 _) <- selectFirst [PersonName ==. "Key"] []
      k2 @== k

  it "insertEntity" $ runDb $ do
      Entity k p <- insertEntity $ Person "name" 1 Nothing
      Just p2 <- get k
      p2 @== p

  it "insertRecord" $ runDb $ do
      let record = Person "name" 1 Nothing
      record' <- insertRecord record
      record' @== record

  it "getEntity" $ runDb $ do
      Entity k p <- insertEntity $ Person "name" 1 Nothing
      Just (Entity k2 p2) <- getEntity k
      p @== p2
      k @== k2

  it "getJustEntity" $ runDb $ do
      let p1 = Person "name" 1 Nothing
      k1 <- insert p1
      Entity k2 p2 <- getJustEntity k1
      p1 @== p2
      k1 @== k2

  it "repsert" $ runDb $ do
      k <- liftIO (PersonKey `fmap` generateKey)
      Nothing <- selectFirst [PersonName ==. "Repsert"] []
      repsert k $ Person "Repsert" 26 Nothing
      Just (Entity k2 _) <- selectFirst [PersonName ==. "Repsert"] []
      k2 @== k
      repsert k $ Person "Repsert" 27 Nothing
      Just (Entity k3 p) <- selectFirst [PersonName ==. "Repsert"] []
      k3 @== k
      27 @== personAge p

  it "retrieves a belongsToJust association" $ runDb $ do
      let p = Person "pet owner" 30 Nothing
      person <- insert p
      let cat = Pet person "Mittens" Cat
      p2 <- getJust $ petOwnerId cat
      p @== p2
      p3 <- belongsToJust petOwnerId cat
      p @== p3

  it "retrieves a belongsTo association" $ runDb $ do
      let p = Person "pet owner" 30 Nothing
      person <- insert p
      let cat = MaybeOwnedPet (Just person) "Mittens" Cat
      p2 <- getJust $ fromJust $ maybeOwnedPetOwnerId cat
      p @== p2
      Just p4 <- belongsTo maybeOwnedPetOwnerId cat
      p @== p4

  it "derivePersistField" $ runDb $ do
      person <- insert $ Person "pet owner" 30 Nothing
      catKey <- insert $ Pet person "Mittens" Cat
      Just cat' <- get catKey
      liftIO $ petType cat' @?= Cat
      dog <- insert $ Pet person "Spike" Dog
      Just dog' <- get dog
      liftIO $ petType dog' @?= Dog

  it "derivePersistFieldJSON" $ runDb $ do
      let mittensCollar = PetCollar "Mittens\n1-714-668-9672" True
      pkey <- insert $ Person "pet owner" 30 Nothing
      catKey <- insert $ OutdoorPet pkey mittensCollar Cat
      Just (OutdoorPet _ collar' _) <- get catKey
      liftIO $ collar' @?= mittensCollar

  it "idIn" $ runDb $ do
      let p1 = Person "D" 0 Nothing
          p2 = Person "E" 1 Nothing
          p3 = Person "F" 2 Nothing
      pid1 <- insert p1
      _ <- insert p2
      pid3 <- insert p3
      x <- selectList [PersonId <-. [pid1, pid3]] []
      liftIO $ x @?= [Entity pid1 p1, Entity pid3 p3]

  it "In" $ runDb $ do
      let p1 = Person "D" 0 Nothing
          p2 = Person "E" 1 Nothing
          p3 = Person "F" 2 (Just "blue")
      insert_ p1
      insert_ p2
      insert_ p3
      x1 <- fmap entityVal `fmap` selectList [PersonName <-. ["D"]] []
      liftIO $ x1 @?= [p1]
      x2 <- fmap entityVal `fmap` selectList [PersonName /<-. ["D"]] []
      liftIO $ x2 @?= [p2, p3]

      x3 <- fmap entityVal `fmap` selectList [PersonColor <-. [Just "blue"]] []
      liftIO $ x3 @?= [p3]
      x4 <- fmap entityVal `fmap` selectList [PersonColor /<-. [Just "blue"]] []
      liftIO $ x4 @?= [p1, p2]

      x5 <- fmap entityVal `fmap` selectList [PersonColor <-. [Nothing, Just "blue"]] []
      liftIO $ x5 @?= [p1, p2, p3]
      x6 <- fmap entityVal `fmap` selectList [PersonColor /<-. [Nothing]] []
      liftIO $ x6 @?= [p3]

  describe "toJSON" $ do
    it "serializes" $ runDb $ do
      let p = Person "D" 0 Nothing
      k <- insert p
      liftIO $ toJSON (Entity k p) @?=
        Object (M.fromList [("id", toJSON k), ("color",Null),("name",String "D"),("age",Number 0)])

{- FIXME
    prop "fromJSON . toJSON $ key" $ \(person :: Key Person) ->
      case (fromJSON . toJSON) person of
        Success p -> p == person
        _ -> error "fromJSON"
-}

  describe "strictness" $ do
    it "bang" $ (return $! Strict (error "foo") 5 5) `shouldThrow` anyErrorCall
    it "tilde" $ void (return $! Strict 5 (error "foo") 5 :: IO Strict)
    it "blank" $ (return $! Strict 5 5 (error "foo")) `shouldThrow` anyErrorCall

  describe "documentation syntax" $ do
    let edef = entityDef (Proxy :: Proxy Relationship)
    it "provides comments on entity def" $ do
      entityComments edef
        `shouldBe`
          Just "This is a doc comment for a relationship.\nYou need to put the pipe character for each line of documentation.\nBut you can resume the doc comments afterwards.\n"
    it "provides comments on the field" $ do
      let [nameField, parentField] = entityFields edef
      fieldComments nameField
        `shouldBe`
          Just "Fields should be documentable.\n"

  describe "JsonEncoding" $ do
    let
      subject =
        JsonEncoding "Bob" 32
      subjectEntity =
        Entity (JsonEncodingKey (jsonEncodingName subject)) subject

    it "encodes without an ID field" $ do
      toJSON subjectEntity
        `shouldBe`
          Object (M.fromList
            [ ("name", String "Bob")
            , ("age", toJSON (32 :: Int))
            , ("id", String "Bob")
            ])

    it "decodes without an ID field" $ do
      let
        json = encode . Object . M.fromList $
          [ ("name", String "Bob")
          , ("age", toJSON (32 :: Int))
          ]
      decode json
        `shouldBe`
          Just subjectEntity

    prop "works with a Primary" $ \jsonEncoding -> do
      let
        ent =
          Entity (JsonEncodingKey (jsonEncodingName jsonEncoding)) jsonEncoding
      decode (encode ent)
        `shouldBe`
          Just ent

    prop "excuse me what" $ \j@JsonEncoding{..} -> do
      let
        ent =
          Entity (JsonEncodingKey jsonEncodingName) j
      toJSON ent
        `shouldBe`
          Object (M.fromList
            [ ("name", toJSON jsonEncodingName)
            , ("age", toJSON jsonEncodingAge)
            , ("id", toJSON jsonEncodingName)
            ])

    prop "round trip works with composite key" $ \j@JsonEncoding2{..} -> do
      let
        key = JsonEncoding2Key jsonEncoding2Name jsonEncoding2Blood
        ent =
          Entity key j
      decode (encode ent)
        `shouldBe`
          Just ent

    prop "works with a composite key" $ \j@JsonEncoding2{..} -> do
      let
        key = JsonEncoding2Key jsonEncoding2Name jsonEncoding2Blood
        ent =
          Entity key j
      toJSON ent
        `shouldBe`
          Object (M.fromList
            [ ("name", toJSON jsonEncoding2Name)
            , ("age", toJSON jsonEncoding2Age)
            , ("blood", toJSON jsonEncoding2Blood)
            , ("id", toJSON key)
            ])


