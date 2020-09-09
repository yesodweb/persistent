{-# LANGUAGE OverloadedStrings #-}

-- | This type is taken directly from the example given here: https://hackage.haskell.org/package/persistent-2.10.5/docs/Database-Persist-Types.html#t:PersistValue
module GeographyType where

import PgInit
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

data Geo = Geo ByteString deriving (Show, Eq)

instance PersistField Geo where
  toPersistValue (Geo t) = PersistDbSpecific t

  fromPersistValue (PersistDbSpecific t) = Right $ Geo $ B.concat ["'", t, "'"]
  fromPersistValue _ = Left "Geo values must be converted from PersistDbSpecific"

instance PersistFieldSql Geo where
  sqlType _ = SqlOther "GEOGRAPHY(POINT,4326)"

toPoint :: Double -> Double -> Geo
toPoint lat lon = Geo $ B.concat ["'POINT(", ps $ lon, " ", ps $ lat, ")'"]
  where ps = B8.pack . show
