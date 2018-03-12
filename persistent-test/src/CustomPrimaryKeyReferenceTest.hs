{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- This test is based on this issue: https://github.com/yesodweb/persistent/issues/421
-- The primary thing this is testing is the migration, thus the test code itself being mostly negligible.
module CustomPrimaryKeyReferenceTest where

import Init

-- mpsGeneric = False is due to a bug or at least lack of a feature in mkKeyTypeDec TH.hs
#if WITH_NOSQL
mkPersist persistSettings { mpsGeneric = False } [persistUpperCase|
#else
share [mkPersist persistSettings { mpsGeneric = False }, mkMigrate "migration"] [persistLowerCase|
#endif
  Tweet
    tweetId Int
    statusText Text sqltype=varchar(170)
    Primary tweetId
    UniqueTweetId tweetId
    deriving Show
  TweetUrl
    tweetId TweetId
    tweetUrl Text sqltype=varchar(255)
    finalUrl Text Maybe sqltype=varchar(255)
    UniqueTweetIdTweetUrl tweetId tweetUrl
    deriving Show
|]
#ifdef WITH_NOSQL
cleanDB :: (MonadIO m, PersistQuery backend, PersistEntityBackend Tweet ~ backend) => ReaderT backend m ()
cleanDB = do
  deleteWhere ([] :: [Filter Tweet])
  deleteWhere ([] :: [Filter TweetUrl])

db :: Action IO () -> Assertion
db = db' cleanDB
#endif

specs :: Spec
specs = describe "custom primary key reference" $ do
#ifdef WITH_NOSQL
  return ()
#else

  let tweet = Tweet {tweetTweetId = 1, tweetStatusText = "Hello!"}

  it "can insert a Tweet" $ db $ do
    tweetId <- insert tweet
    let url = TweetUrl {tweetUrlTweetId = tweetId, tweetUrlTweetUrl = "http://google.com", tweetUrlFinalUrl = Just "http://example.com"}
    insert_ url

  return ()

#endif
