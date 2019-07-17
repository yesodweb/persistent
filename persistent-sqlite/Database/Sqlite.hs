{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A port of the direct-sqlite package for dealing directly with
-- 'PersistValue's.
module Database.Sqlite  (
                         Connection,
                         Statement,
                         Error(..),
                         SqliteException(..),
                         StepResult(Row, Done),
                         Config(ConfigLogFn),
                         LogFunction,
                         SqliteStatus (..),
                         SqliteStatusVerb (..),
    -- * Basic usage guide
    -- |
    --
    -- Note that the example code shown here is a low level interface
    -- usage. Let's create a small demo sqlite3 database which we will
    -- use in our program:
    --
    -- > $ sqlite3 ~/test.db
    -- > sqlite> create table t1(a,b);
    -- > sqlite> insert into t1(a,b) values (1,1);
    -- > sqlite> insert into t1(a,b) values (2,2);
    -- > sqlite> select * from t1;
    -- > 1|1
    -- > 2|2
    --
    -- Now let's write code using the functions in this module to
    -- fetch the rows from the table:
    --
    -- > {-#LANGUAGE OverloadedStrings#-}
    -- >
    -- > import Database.Sqlite
    -- > import Data.Text
    -- >
    -- > main :: IO ()
    -- > main = do
    -- >   conn <- open "/home/sibi/test.db"
    -- >   smt <- prepare conn "select * from t1;"
    -- >   row1 <- step smt >> columns smt
    -- >   row2 <- step smt >> columns smt
    -- >   print (row1, row2)
    -- >   finalize smt
    -- >   close conn
    --
    -- On executing the above code:
    --
    -- > $ ./demo-program
    -- > $ ([PersistInt64 1,PersistInt64 1],[PersistInt64 2,PersistInt64 2])

                         open,
                         close,
                         prepare,
                         step,
                         stepConn,
                         reset,
                         finalize,
                         bindBlob,
                         bindDouble,
                         bindInt,
                         bindInt64,
                         bindNull,
                         bindText,
                         bind,
                         column,
                         columns,
                         changes,
                         mkLogFunction,
                         freeLogFunction,
                         config,
                         status,
                         softHeapLimit,
                         enableExtendedResultCodes,
                         disableExtendedResultCodes
                        )
    where

import Prelude hiding (error)
import qualified Prelude as P

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Internal as BSI
import Data.Fixed (Pico)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Monoid (mappend, mconcat)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (defaultTimeLocale, formatTime, UTCTime)
import Data.Typeable (Typeable)
import Database.Sqlite.Internal (Connection(..), Connection'(..), Statement(..))
import Foreign
import Foreign.C

import Database.Persist (PersistValue (..), listToJSON, mapToJSON)

-- | A custom exception type to make it easier to catch exceptions.
--
-- @since 2.1.3
data SqliteException = SqliteException
    { seError        :: !Error
    , seFunctionName :: !Text
    , seDetails      :: !Text
    }
    deriving (Typeable)
instance Show SqliteException where
    show (SqliteException error functionName details) = unpack $ Data.Monoid.mconcat
        ["SQLite3 returned "
        , pack $ show error
        , " while attempting to perform "
        , functionName
        , details
        ]
instance Exception SqliteException

data Error = ErrorOK
           | ErrorError
           | ErrorInternal
           | ErrorPermission
           | ErrorAbort
           | ErrorBusy
           | ErrorLocked
           | ErrorNoMemory
           | ErrorReadOnly
           | ErrorInterrupt
           | ErrorIO
           | ErrorNotFound
           | ErrorCorrupt
           | ErrorFull
           | ErrorCan'tOpen
           | ErrorProtocol
           | ErrorEmpty
           | ErrorSchema
           | ErrorTooBig
           | ErrorConstraint
           | ErrorMismatch
           | ErrorMisuse
           | ErrorNoLargeFileSupport
           | ErrorAuthorization
           | ErrorFormat
           | ErrorRange
           | ErrorNotAConnection
           | ErrorRow
           | ErrorDone
             deriving (Eq, Show)

data StepResult = Row | Done deriving (Eq, Show)

data ColumnType = IntegerColumn
                | FloatColumn
                | TextColumn
                | BlobColumn
                | NullColumn
                  deriving (Eq, Show)

decodeError :: Int -> Error
decodeError 0 = ErrorOK
decodeError 1 = ErrorError
decodeError 2 = ErrorInternal
decodeError 3 = ErrorPermission
decodeError 4 = ErrorAbort
decodeError 5 = ErrorBusy
decodeError 6 = ErrorLocked
decodeError 7 = ErrorNoMemory
decodeError 8 = ErrorReadOnly
decodeError 9 = ErrorInterrupt
decodeError 10 = ErrorIO
decodeError 11 = ErrorNotFound
decodeError 12 = ErrorCorrupt
decodeError 13 = ErrorFull
decodeError 14 = ErrorCan'tOpen
decodeError 15 = ErrorProtocol
decodeError 16 = ErrorEmpty
decodeError 17 = ErrorSchema
decodeError 18 = ErrorTooBig
decodeError 19 = ErrorConstraint
decodeError 20 = ErrorMismatch
decodeError 21 = ErrorMisuse
decodeError 22 = ErrorNoLargeFileSupport
decodeError 23 = ErrorAuthorization
decodeError 24 = ErrorFormat
decodeError 25 = ErrorRange
decodeError 26 = ErrorNotAConnection
decodeError 100 = ErrorRow
decodeError 101 = ErrorDone
decodeError i = P.error $ "decodeError " ++ show i

decodeColumnType :: Int -> ColumnType
decodeColumnType 1 = IntegerColumn
decodeColumnType 2 = FloatColumn
decodeColumnType 3 = TextColumn
decodeColumnType 4 = BlobColumn
decodeColumnType 5 = NullColumn
decodeColumnType i = P.error $ "decodeColumnType " ++ show i

foreign import ccall "sqlite3_errmsg"
  errmsgC :: Ptr () -> IO CString
errmsg :: Connection -> IO Text
errmsg (Connection _ (Connection' database)) = do
  message <- errmsgC database
  byteString <- BS.packCString message
  return $ decodeUtf8With lenientDecode byteString

sqlError :: Maybe Connection -> Text -> Error -> IO a
sqlError maybeConnection functionName error = do
  details <- case maybeConnection of
               Just database -> do
                 details <- errmsg database
                 return $ ": " `Data.Monoid.mappend` details
               Nothing -> return "."
  throwIO SqliteException
    { seError = error
    , seFunctionName = functionName
    , seDetails = details
    }

foreign import ccall "sqlite3_open_v2"
  openC :: CString -> Ptr (Ptr ()) -> Int -> CString -> IO Int

openError :: Text -> IO (Either Connection Error)
openError path' = do
    let flag = sqliteFlagReadWrite .|. sqliteFlagCreate .|. sqliteFlagUri
    BS.useAsCString (encodeUtf8 path') $ \path -> alloca $ \database -> do
        err <- decodeError <$> openC path database flag nullPtr
        case err of
            ErrorOK -> do database' <- peek database
                          active <- newIORef True
                          return $ Left $ Connection active $ Connection' database'
            _ -> return $ Right err
  where
    -- for all sqlite flags, check out https://www.sqlite.org/c3ref/open.html
    sqliteFlagReadWrite = 0x2
    sqliteFlagCreate    = 0x4
    sqliteFlagUri       = 0x40

open :: Text -> IO Connection
open path = do
  databaseOrError <- openError path
  case databaseOrError of
    Left database -> return database
    Right error -> sqlError Nothing ("open " `mappend` (pack $ show path)) error

foreign import ccall "sqlite3_close"
  closeC :: Ptr () -> IO Int
closeError :: Connection -> IO Error
closeError (Connection iactive (Connection' database)) = do
  writeIORef iactive False
  error <- closeC database
  return $ decodeError error
close :: Connection -> IO ()
close database = do
  error <- closeError database
  case error of
    ErrorOK -> return ()
    _ -> sqlError (Just database) "close" error

foreign import ccall "sqlite3_extended_result_codes"
  sqlite3_extended_result_codesC :: Ptr () -> Int -> IO Int


-- @since 2.9.2
enableExtendedResultCodes :: Connection -> IO ()
enableExtendedResultCodes con@(Connection _ (Connection' database)) =  do
  error <- sqlite3_extended_result_codesC database 1
  let err = decodeError error
  case err of
    ErrorOK -> return ()
    _ -> sqlError (Just con) "enableExtendedResultCodes" err

-- @since 2.9.2
disableExtendedResultCodes :: Connection -> IO ()
disableExtendedResultCodes con@(Connection _ (Connection' database)) =  do
  error <- sqlite3_extended_result_codesC database 0
  let err = decodeError error
  case err of
    ErrorOK -> return ()
    _ -> sqlError (Just con) "disableExtendedResultCodes" err

foreign import ccall "sqlite3_prepare_v2"
  prepareC :: Ptr () -> CString -> Int -> Ptr (Ptr ()) -> Ptr (Ptr ()) -> IO Int
prepareError :: Connection -> Text -> IO (Either Statement Error)
prepareError (Connection _ (Connection' database)) text' = do
  BS.useAsCString (encodeUtf8 text')
                  (\text -> do
                     alloca (\statement -> do
                               error' <- prepareC database text (-1) statement nullPtr
                               error <- return $ decodeError error'
                               case error of
                                 ErrorOK -> do
                                            statement' <- peek statement
                                            return $ Left $ Statement statement'
                                 _ -> return $ Right error))
prepare :: Connection -> Text -> IO Statement
prepare database text = do
  statementOrError <- prepareError database text
  case statementOrError of
    Left statement -> return statement
    Right error -> sqlError (Just database) ("prepare " `mappend` (pack $ show text)) error

foreign import ccall "sqlite3_step"
  stepC :: Ptr () -> IO Int
stepError :: Statement -> IO Error
stepError (Statement statement) = do
  error <- stepC statement
  return $ decodeError error

-- | Execute a database statement. It's recommended to use 'stepConn' instead, because it gives better error messages.
step :: Statement -> IO StepResult
step statement = do
  error <- stepError statement
  case error of
    ErrorRow -> return Row
    ErrorDone -> return Done
    _ -> sqlError Nothing "step" error

-- | Execute a database statement. This function uses the 'Connection' passed to it to give better error messages than 'step'.
--
-- @since 2.6.4
stepConn :: Connection -> Statement -> IO StepResult
stepConn database statement = do
  error <- stepError statement
  case error of
    ErrorRow -> return Row
    ErrorDone -> return Done
    _ -> sqlError (Just database) "step" error

foreign import ccall "sqlite3_reset"
  resetC :: Ptr () -> IO Int
resetError :: Statement -> IO Error
resetError (Statement statement) = do
  error <- resetC statement
  return $ decodeError error
reset :: Connection -> Statement -> IO ()
reset (Connection iactive _) statement = do
  active <- readIORef iactive
  if active
      then do
          error <- resetError statement
          case error of
            ErrorOK -> return ()
            _ -> return () -- FIXME confirm this is correct sqlError Nothing "reset" error
      else return ()

foreign import ccall "sqlite3_finalize"
  finalizeC :: Ptr () -> IO Int
finalizeError :: Statement -> IO Error
finalizeError (Statement statement) = do
  error <- finalizeC statement
  return $ decodeError error
finalize :: Statement -> IO ()
finalize statement = do
  error <- finalizeError statement
  case error of
    ErrorOK -> return ()
    _ -> return () -- sqlError Nothing "finalize" error

-- Taken from: https://github.com/IreneKnapp/direct-sqlite/blob/master/Database/SQLite3/Direct.hs
-- | Like 'unsafeUseAsCStringLen', but if the string is empty,
-- never pass the callback a null pointer.
unsafeUseAsCStringLenNoNull
    :: BS.ByteString
    -> (CString -> Int -> IO a)
    -> IO a
unsafeUseAsCStringLenNoNull bs cb
    | BS.null bs = cb (intPtrToPtr 1) 0
    | otherwise = BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
        cb ptr (fromIntegral len)

foreign import ccall "sqlite3_bind_blob"
  bindBlobC :: Ptr () -> Int -> Ptr () -> Int -> Ptr () -> IO Int
bindBlobError :: Statement -> Int -> BS.ByteString -> IO Error
bindBlobError (Statement statement) parameterIndex byteString =
  unsafeUseAsCStringLenNoNull byteString $ \dataC size -> do
    error <- bindBlobC statement parameterIndex (castPtr dataC) size
                                        (intPtrToPtr (-1))
    return $ decodeError error
bindBlob :: Statement -> Int -> BS.ByteString -> IO ()
bindBlob statement parameterIndex byteString = do
  error <- bindBlobError statement parameterIndex byteString
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind blob" error

foreign import ccall "sqlite3_bind_double"
  bindDoubleC :: Ptr () -> Int -> Double -> IO Int
bindDoubleError :: Statement -> Int -> Double -> IO Error
bindDoubleError (Statement statement) parameterIndex datum = do
  error <- bindDoubleC statement parameterIndex datum
  return $ decodeError error
bindDouble :: Statement -> Int -> Double -> IO ()
bindDouble statement parameterIndex datum = do
  error <- bindDoubleError statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind double" error

foreign import ccall "sqlite3_bind_int"
  bindIntC :: Ptr () -> Int -> Int -> IO Int
bindIntError :: Statement -> Int -> Int -> IO Error
bindIntError (Statement statement) parameterIndex datum = do
  error <- bindIntC statement parameterIndex datum
  return $ decodeError error
bindInt :: Statement -> Int -> Int -> IO ()
bindInt statement parameterIndex datum = do
  error <- bindIntError statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind int" error

foreign import ccall "sqlite3_bind_int64"
  bindInt64C :: Ptr () -> Int -> Int64 -> IO Int
bindInt64Error :: Statement -> Int -> Int64 -> IO Error
bindInt64Error (Statement statement) parameterIndex datum = do
  error <- bindInt64C statement parameterIndex datum
  return $ decodeError error
bindInt64 :: Statement -> Int -> Int64 -> IO ()
bindInt64 statement parameterIndex datum = do
  error <- bindInt64Error statement parameterIndex datum
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind int64" error

foreign import ccall "sqlite3_bind_null"
  bindNullC :: Ptr () -> Int -> IO Int
bindNullError :: Statement -> Int -> IO Error
bindNullError (Statement statement) parameterIndex = do
  error <- bindNullC statement parameterIndex
  return $ decodeError error
bindNull :: Statement -> Int -> IO ()
bindNull statement parameterIndex = do
  error <- bindNullError statement parameterIndex
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind null" error

foreign import ccall "sqlite3_bind_text"
  bindTextC :: Ptr () -> Int -> CString -> Int -> Ptr () -> IO Int
bindTextError :: Statement -> Int -> Text -> IO Error
bindTextError (Statement statement) parameterIndex text =
  unsafeUseAsCStringLenNoNull (encodeUtf8 text) $ \dataC size -> do
    error <- bindTextC statement parameterIndex dataC size (intPtrToPtr (-1))
    return $ decodeError error
bindText :: Statement -> Int -> Text -> IO ()
bindText statement parameterIndex text = do
  error <- bindTextError statement parameterIndex text
  case error of
    ErrorOK -> return ()
    _ -> sqlError Nothing "bind text" error

bind :: Statement -> [PersistValue] -> IO ()
bind statement sqlData = do
  mapM_ (\(parameterIndex, datum) -> do
          case datum of
            PersistInt64 int64 -> bindInt64 statement parameterIndex int64
            PersistDouble double -> bindDouble statement parameterIndex double
            PersistRational rational -> bindText statement parameterIndex $ pack $ show (fromRational rational :: Pico)
            PersistBool b -> bindInt64 statement parameterIndex $
                                if b then 1 else 0
            PersistText text -> bindText statement parameterIndex text
            PersistByteString blob -> bindBlob statement parameterIndex blob
            PersistNull -> bindNull statement parameterIndex
            PersistDay d -> bindText statement parameterIndex $ pack $ show d
            PersistTimeOfDay d -> bindText statement parameterIndex $ pack $ show d
            PersistUTCTime d -> bindText statement parameterIndex $ pack $ format8601 d
            PersistList l -> bindText statement parameterIndex $ listToJSON l
            PersistMap m -> bindText statement parameterIndex $ mapToJSON m
            PersistDbSpecific s -> bindText statement parameterIndex $ decodeUtf8With lenientDecode s
            PersistArray a -> bindText statement parameterIndex $ listToJSON a -- copy of PersistList's definition
            PersistObjectId _ -> P.error "Refusing to serialize a PersistObjectId to a SQLite value"
            )
       $ zip [1..] sqlData
  return ()

format8601 :: UTCTime -> String
format8601 = formatTime defaultTimeLocale "%FT%T%Q"

foreign import ccall "sqlite3_column_type"
  columnTypeC :: Ptr () -> Int -> IO Int
columnType :: Statement -> Int -> IO ColumnType
columnType (Statement statement) columnIndex = do
  result <- columnTypeC statement columnIndex
  return $ decodeColumnType result

foreign import ccall "sqlite3_column_bytes"
  columnBytesC :: Ptr () -> Int -> IO Int

foreign import ccall "sqlite3_column_blob"
  columnBlobC :: Ptr () -> Int -> IO (Ptr ())
columnBlob :: Statement -> Int -> IO BS.ByteString
columnBlob (Statement statement) columnIndex = do
  size <- columnBytesC statement columnIndex
  BSI.create size (\resultPtr -> do
                     dataPtr <- columnBlobC statement columnIndex
                     if dataPtr /= nullPtr
                        then BSI.memcpy resultPtr (castPtr dataPtr) (fromIntegral size)
                        else return ())

foreign import ccall "sqlite3_column_int64"
  columnInt64C :: Ptr () -> Int -> IO Int64
columnInt64 :: Statement -> Int -> IO Int64
columnInt64 (Statement statement) columnIndex = do
  columnInt64C statement columnIndex

foreign import ccall "sqlite3_column_double"
  columnDoubleC :: Ptr () -> Int -> IO Double
columnDouble :: Statement -> Int -> IO Double
columnDouble (Statement statement) columnIndex = do
  columnDoubleC statement columnIndex

foreign import ccall "sqlite3_column_text"
  columnTextC :: Ptr () -> Int -> IO CString
columnText :: Statement -> Int -> IO Text
columnText (Statement statement) columnIndex = do
  text <- columnTextC statement columnIndex
  len <- columnBytesC statement columnIndex
  byteString <- BS.packCStringLen (text, len)
  return $ decodeUtf8With lenientDecode byteString

foreign import ccall "sqlite3_column_count"
  columnCountC :: Ptr () -> IO Int
columnCount :: Statement -> IO Int
columnCount (Statement statement) = do
  columnCountC statement

column :: Statement -> Int -> IO PersistValue
column statement columnIndex = do
  theType <- columnType statement columnIndex
  case theType of
    IntegerColumn -> do
                 int64 <- columnInt64 statement columnIndex
                 return $ PersistInt64 int64
    FloatColumn -> do
                 double <- columnDouble statement columnIndex
                 return $ PersistDouble double
    TextColumn -> do
                 text <- columnText statement columnIndex
                 return $ PersistText text
    BlobColumn -> do
                 byteString <- columnBlob statement columnIndex
                 return $ PersistByteString byteString
    NullColumn -> return PersistNull

columns :: Statement -> IO [PersistValue]
columns statement = do
  count <- columnCount statement
  mapM (\i -> column statement i) [0..count-1]

foreign import ccall "sqlite3_changes"
  changesC :: Connection' -> IO Int

changes :: Connection -> IO Int64
changes (Connection _ c) = fmap fromIntegral $ changesC c

-- | Log function callback. Arguments are error code and log message.
--
-- @since 2.1.4
type RawLogFunction = Ptr () -> Int -> CString -> IO ()

foreign import ccall "wrapper"
  mkRawLogFunction :: RawLogFunction -> IO (FunPtr RawLogFunction)

-- |
-- @since 2.1.4
newtype LogFunction = LogFunction (FunPtr RawLogFunction)

-- | Wraps a given function to a 'LogFunction' to be further used with 'ConfigLogFn'.
-- First argument of given function will take error code, second - log message.
-- Returned value should be released with 'freeLogFunction' when no longer required.
mkLogFunction :: (Int -> String -> IO ()) -> IO LogFunction
mkLogFunction fn = fmap LogFunction . mkRawLogFunction $ \_ errCode cmsg -> do
  msg <- peekCString cmsg
  fn errCode msg

-- | Releases a native FunPtr for the 'LogFunction'.
--
-- @since 2.1.4
freeLogFunction :: LogFunction -> IO ()
freeLogFunction (LogFunction fn) = freeHaskellFunPtr fn

-- | Configuration option for SQLite to be used together with the 'config' function.
--
-- @since 2.1.4
data Config
  -- | A function to be used for logging
  = ConfigLogFn LogFunction

foreign import ccall "persistent_sqlite_set_log"
  set_logC :: FunPtr RawLogFunction -> Ptr () -> IO Int

-- | Sets SQLite global configuration parameter. See SQLite documentation for the <https://www.sqlite.org/c3ref/config.html sqlite3_config> function.
-- In short, this must be called prior to any other SQLite function if you want the call to succeed.
--
-- @since 2.1.4
config :: Config -> IO ()
config c = case c of
  ConfigLogFn (LogFunction rawLogFn) -> do
    e <- fmap decodeError $ set_logC rawLogFn nullPtr
    case e of
      ErrorOK -> return ()
      _ -> sqlError Nothing "sqlite3_config" e

-- | Return type of the 'status' function
--
-- @since 2.6.1
data SqliteStatus = SqliteStatus
  { sqliteStatusCurrent   :: Maybe Int
  -- ^ The current value of the parameter. Some parameters do not record current value.
  , sqliteStatusHighwater :: Maybe Int
  -- ^ The highest recorded value. Some parameters do not record the highest value.
  } deriving (Eq, Show)

-- | Run-time status parameter that can be returned by 'status' function.
--
-- @since 2.6.1
data SqliteStatusVerb
  -- | This parameter is the current amount of memory checked out using sqlite3_malloc(),
  -- either directly or indirectly. The figure includes calls made to sqlite3_malloc()
  -- by the application and internal memory usage by the SQLite library. Scratch memory
  -- controlled by SQLITE_CONFIG_SCRATCH and auxiliary page-cache memory controlled by
  -- SQLITE_CONFIG_PAGECACHE is not included in this parameter. The amount returned is
  -- the sum of the allocation sizes as reported by the xSize method in sqlite3_mem_methods.
  = SqliteStatusMemoryUsed
  -- | This parameter returns the number of pages used out of the pagecache memory
  -- allocator that was configured using SQLITE_CONFIG_PAGECACHE. The value returned
  -- is in pages, not in bytes.
  | SqliteStatusPagecacheUsed
  -- | This parameter returns the number of bytes of page cache allocation which
  -- could not be satisfied by the SQLITE_CONFIG_PAGECACHE buffer and where forced
  -- to overflow to sqlite3_malloc(). The returned value includes allocations that
  -- overflowed because they where too large (they were larger than the "sz"
  -- parameter to SQLITE_CONFIG_PAGECACHE) and allocations that overflowed because
  -- no space was left in the page cache.
  | SqliteStatusPagecacheOverflow
  -- | This parameter returns the number of allocations used out of the scratch
  -- memory allocator configured using SQLITE_CONFIG_SCRATCH. The value returned
  -- is in allocations, not in bytes. Since a single thread may only have one
  -- scratch allocation outstanding at time, this parameter also reports the
  -- number of threads using scratch memory at the same time.
  | SqliteStatusScratchUsed
  -- | This parameter returns the number of bytes of scratch memory allocation
  -- which could not be satisfied by the SQLITE_CONFIG_SCRATCH buffer and where
  -- forced to overflow to sqlite3_malloc(). The values returned include overflows
  -- because the requested allocation was too larger (that is, because the requested
  -- allocation was larger than the "sz" parameter to SQLITE_CONFIG_SCRATCH) and
  -- because no scratch buffer slots were available.
  | SqliteStatusScratchOverflow
  -- | This parameter records the largest memory allocation request handed to
  -- sqlite3_malloc() or sqlite3_realloc() (or their internal equivalents). Only
  -- the value returned in 'sqliteStatusHighwater' field of 'SqliteStatus' record
  -- is of interest. The value written into the 'sqliteStatusCurrent' field is Nothing.
  | SqliteStatusMallocSize
  -- | This parameter records the largest memory allocation request handed to
  -- pagecache memory allocator. Only the value returned in the 'sqliteStatusHighwater'
  -- field of 'SqliteStatus' record is of interest. The value written into the
  -- 'sqliteStatusCurrent' field is Nothing.
  | SqliteStatusPagecacheSize
  -- | This parameter records the largest memory allocation request handed to
  -- scratch memory allocator. Only the value returned in the 'sqliteStatusHighwater'
  -- field of 'SqliteStatus' record is of interest. The value written into the
  -- 'sqliteStatusCurrent' field is Nothing.
  | SqliteStatusScratchSize
  -- | This parameter records the number of separate memory allocations currently
  -- checked out.
  | SqliteStatusMallocCount

-- Internal function to convert status parameter to a triple of its integral
-- constant and two bools indicating if native sqlite3_status function actually
-- modifies values at pCurrent and pHighwater pointers.
statusVerbInfo :: SqliteStatusVerb -> (CInt, Bool, Bool)
statusVerbInfo v = case v of
  SqliteStatusMemoryUsed -> (0, True, True)
  SqliteStatusPagecacheUsed -> (1, True, True)
  SqliteStatusPagecacheOverflow -> (2, True, True)
  SqliteStatusScratchUsed -> (3, True, True)
  SqliteStatusScratchOverflow -> (4, True, True)
  SqliteStatusMallocSize -> (5, False, True)
  SqliteStatusPagecacheSize -> (7, False, True)
  SqliteStatusScratchSize -> (8, False, True)
  SqliteStatusMallocCount -> (9, True, True)

foreign import ccall "sqlite3_status"
  statusC :: CInt -> Ptr CInt -> Ptr CInt -> CInt -> IO Int

-- | Retrieves runtime status information about the performance of SQLite,
-- and optionally resets various highwater marks. The first argument is a
-- status parameter to measure, the second is reset flag. If reset flag is
-- True then the highest recorded value is reset after being returned from
-- this function.
--
-- @since 2.6.1
status :: SqliteStatusVerb -> Bool -> IO SqliteStatus
status verb reset' = alloca $ \pCurrent -> alloca $ \pHighwater -> do
  let (code, hasCurrent, hasHighwater) = statusVerbInfo verb
  e <- decodeError <$> statusC code pCurrent pHighwater (if reset' then 1 else 0)
  case e of
    ErrorOK -> do
      current <- if hasCurrent then Just . fromIntegral <$> peek pCurrent else return Nothing
      highwater <- if hasHighwater then Just . fromIntegral <$> peek pHighwater else return Nothing
      return $ SqliteStatus current highwater
    _ -> sqlError Nothing "sqlite3_status" e

foreign import ccall "sqlite3_soft_heap_limit64"
  softHeapLimit64C :: CLLong -> IO CLLong

-- | Sets and/or queries the soft limit on the amount of heap memory that may be
-- allocated by SQLite. If the argument is zero then the soft heap limit is disabled.
-- If the argument is negative then no change is made to the soft heap limit. Hence,
-- the current size of the soft heap limit can be determined by invoking
-- this function with a negative argument.
--
-- @since 2.6.1
softHeapLimit :: Int64 -> IO Int64
softHeapLimit x = fromIntegral <$> softHeapLimit64C (CLLong x)
