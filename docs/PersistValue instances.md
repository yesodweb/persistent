These are orphan instances that don't have a home yet.

``` haskell
-- The Scientific type, in widespread use by aeson users and useful for financial, scientific data
instance PersistField Scientific where
  toPersistValue = PersistRational . toRational
  fromPersistValue (PersistRational r) = Right $ fromRational r
  fromPersistValue (PersistDouble d) = Right $ fromFloatDigits d
  fromPersistValue (PersistInt64 i) = Right $ fromIntegral i
  fromPersistValue x = Left $ T.pack "PersistField Scientific: Expected Scientific, received: " <> T.pack (show x)

instance PersistFieldSql Scientific where
  sqlType _ = SqlNumeric 32 20  -- Use your own judgement here
```

```haskell
import Numeric (showHex)

-- Store a large integer in hexadecimal format (which is easier to inspect in the database)
instance PersistField Integer where
  toPersistValue i = PersistText . T.pack $ showHex i ""
  fromPersistValue (PersistText s) = read $ "0x" <> T.unpack s
  fromPersistValue x = Left $ T.pack "PersistField Integer: Expected hexadecimal text, received: " <> T.pack (show x)
instance PersistFieldSql Integer where
  sqlType _ = SqlString

-- Alternatively, for a fixed length integer, pad the hexadecimal with zeros
showHexFixed :: (Integral a, Show a) => Int -> a -> String  
showHexFixed len val = padZeros $ showHex val ""
    where padZeros s = if length s >= len then s else padZeros ('0' : s)

instance PersistField Integer where
  toPersistValue i = PersistText . T.pack $ showHexFixed 30 i
  fromPersistValue (PersistText s) = read $ "0x" <> T.unpack s
  fromPersistValue x = Left $ T.pack "PersistField Integer: Expected hexadecimal char(30), received: " <> T.pack (show x)
instance PersistFieldSql Integer where
  sqlType _ = SqlOther $ T.pack "char(30)" -- fixed length character string, stored inline
```