{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Streaming where

import PgInit

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL

share [mkPersist sqlSettings, mkMigrate "streamingMigrate"] [persistLowerCase|

StreamableNumber
    val Int
    deriving Eq Show Ord

|]

numberOfRows :: Int
numberOfRows = 1_000_000

numbers :: Monad m => ConduitT () StreamableNumber m ()
numbers = CC.unfold (\n -> if n >= numberOfRows then Nothing else Just (StreamableNumber n, n + 1)) 1

wipe :: IO ()
wipe = runConnAssert $ do
    deleteWhere ([] :: [Filter StreamableNumber])

itDb :: String -> SqlPersistT (LoggingT (ResourceT IO)) a -> SpecWith (Arg (IO ()))
itDb msg action = it msg $ runConnAssert $ void action

specs :: Spec
specs = describe "Streaming rows" $ do
    describe "selectStream" $ before_ wipe $ do
        itDb "streams rows" $ do
          runConduit $ numbers .| CL.chunksOf 1000 .| CC.mapM_ (void . insertMany)

          let resultStream = selectStream [] [ Asc StreamableNumberVal ]

          let checkIncrementing expected (Entity _ (StreamableNumber actual)) = do
                expected `shouldBe` actual
                pure $ expected + 1

          result <- runConduit $ resultStream .| CC.foldM checkIncrementing 1

          result `shouldBe` numberOfRows
