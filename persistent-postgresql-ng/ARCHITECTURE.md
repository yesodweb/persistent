# persistent-postgresql-ng Architecture

A PostgreSQL backend for the [persistent](https://hackage.haskell.org/package/persistent) library that uses the **binary wire protocol** and **libpq pipeline mode** to reduce round-trips and improve throughput.

## Motivation

The standard `persistent-postgresql` backend sends every operation as a synchronous request-response pair over the text protocol via `postgresql-simple`. For workloads that issue many small DML statements (deletes, updates, replaces) this means one network round-trip per operation.

`persistent-postgresql-ng` changes two things:

1. **Binary protocol**–  parameters and results use PostgreSQL's binary format via `postgresql-binary`, avoiding text serialization overhead.
2. **Automatic pipelining**–  DML operations are sent eagerly without waiting for a response. Results are deferred until a read operation or transaction commit forces them to be consumed.

The pipelining design is inspired by [Hedis](https://hackage.haskell.org/package/hedis), where commands are sent eagerly and replies are read lazily. Instead of lazy IO, this library uses a pending-result counter with explicit drain points.

## Module Overview

```
Database.Persist.Postgresql
├── Pipeline.hs                      -- SqlBackend construction, statement execution, pipeline lifecycle
├── Pipeline/
│   ├── Internal.hs                  -- PgConn type, libpq wrappers, pipeline mode primitives
│   └── FFI.hs                       -- C bindings for chunked-row mode detection
├── Internal.hs                      -- Escape functions, PgInterval, re-exports from Migration
├── Internal/
│   ├── Decoding.hs                  -- Binary result column → PersistValue decoding
│   ├── Encoding.hs                  -- PersistValue → binary parameter encoding
│   ├── DirectDecode.hs              -- PgRowEnv, FieldDecode instances, compositeFieldDecode
│   ├── DirectEncode.hs              -- PgParam ADT, FieldEncode instances
│   ├── PgCodec.hs                   -- PgDecode/PgEncode classes, PgDecoder/PgEncoder reader types, DSL
│   ├── PgType.hs                    -- OID classification (PgType/PgScalar), OidCache
│   ├── Migration.hs                 -- DDL migration logic (adapted from persistent-postgresql)
│   └── Placeholders.hs             -- ? → $1,$2,... placeholder rewriting
├── JSON.hs                          -- JSON column support
└── CustomType.hs                    -- Custom PostgreSQL type support
```

## Connection Lifecycle

### Opening

`openPgConn` (in `Connection.hs`) does three things:

1. Calls `LibPQ.connectdb` to establish a TCP/Unix socket connection.
2. Queries the server version (via `LibPQ.serverVersion` or `SHOW server_version` as fallback).
3. **Enables nonblocking mode** via `LibPQ.setnonblocking`–  required to prevent deadlock in pipeline mode (see below).
4. **Enters pipeline mode** via `LibPQ.enterPipelineMode`–  this stays on for the lifetime of the connection.

The result is a `PgConn`:

```haskell
data PgConn = PgConn
    { pgConn      :: !LibPQ.Connection
    , pgVersion   :: !(NonEmpty Word)
    , pgPending   :: !(IORef Int)
    , pgFetchMode :: !FetchMode
    , pgOidCache  :: !(IORef OidCache)
    }
```

- `pgPending` tracks the number of fire-and-forget query results that have been sent but not yet read.
- `pgFetchMode` controls how result rows are fetched: `FetchAll` (default), `FetchSingleRow`, or `FetchChunked n` (PG17+ libpq).
- `pgOidCache` maps dynamically-assigned OIDs (composites, enums, domains) to `PgType` values. Starts empty; populated at connection time or on first encounter. Used by `FieldDecode` instances for OID validation and by `PgEncode` for composite/enum OID lookup.

### Closing

`closePgConn` drains any pending results (via `pipelineSync` + drain), exits pipeline mode, and calls `LibPQ.finish`.

### Nonblocking Mode and Buffer Management

The connection is set to nonblocking mode before entering pipeline mode. This is critical: in blocking mode, `LibPQ.flush` blocks until the entire send buffer is written to the socket. If the server's send buffer is also full (because we haven't consumed its results), both sides block and deadlock.

In nonblocking mode, `LibPQ.flush` returns `FlushWriting` when the socket buffer is full. Our `pgFlush` handles this by:

1. Calling `threadWaitWrite` (GHC's I/O manager) to wait for socket writability without busy-waiting.
2. Calling `consumeInput` to read any pending server data–  this prevents the server from blocking on *its* send buffer.
3. Retrying `flush` until `FlushOk`.

This cooperative flush loop ensures neither client nor server blocks indefinitely, even under heavy pipeline load.

### Connection Pooling

Each pooled connection has its own independent `pgPending` counter and lazy reply stream. There is no cross-connection pipelining–  operations on one connection do not affect another.

When a connection is returned to the pool, `closePgConn` drains any pending results before closing. If the pool reuses a connection (via `runSqlPool`), persistent's transaction management (`connCommit` / `connRollback`) ensures the pipeline is clean before the connection is returned.

## Pipeline Mode

### How libpq Pipeline Mode Works

In normal (non-pipeline) mode, `execParams` sends a query and blocks until the server responds. In pipeline mode:

- **`sendQueryParams`** queues a query in the client's send buffer without waiting for a response.
- **`pipelineSync`** inserts a sync point–  the server processes all queued queries and sends back results in order.
- **`sendFlushRequest`** asks the server to flush its output buffer without a full sync point.
- **`getResult`** reads the next result from the connection.

Each query's result follows a protocol:
- `getResult` → `Just result` (the actual result)
- `getResult` → `Nothing` (NULL separator marking end of that query's results)

A `pipelineSync` result is different:
- `getResult` → `Just result` with status `PipelineSync` (no NULL separator follows)

### Automatic Pipelining Strategy

Pipeline mode is **always on**. Operations fall into three categories:

**Hedis-style lazy reads** (`pipelinedGet`, `pipelinedInsert`, `pipelinedGetBy`, `pipelinedCount`, `pipelinedExists`):
Sends the query with `sendQueryParams` into the output buffer (no flush), pops a lazy reply from the reply stream (no IO forced). The result is returned as an `unsafeInterleaveIO` thunk. When the caller inspects the value, the thunk fires: flushes the send buffer (sending ALL queued queries in one batch), reads the result. Operations that use this path:

- `get`, `getBy`, `count`, `exists`–  return lazy results
- `insert`–  sends INSERT RETURNING, returns lazy `Key`

This means `mapM get keys` sends all 100 queries before reading any results, achieving **20-29× speedup** at realistic network latencies.

**Fire-and-forget** (via `stmtExecute` → `execute'`):
Sends the query with `sendQueryParams`, increments `pgPending`, returns immediately. No round-trip. Operations that use this path:

- `delete`, `update`, `updateWhere`, `deleteWhere`
- `replace`, `insertKey`, `repsert`, `putMany`
- `rawExecute`, `insert_`

**Conduit-based reads** (via `stmtQuery` → `withStmt'`):
Drains pending fire-and-forget results, then sends the query, pops from the reply stream, and reads the result eagerly (for conduit streaming compatibility). Operations that use this path:

- `selectList`, `selectFirst`, `selectSourceRes`, `selectKeysRes`
- `rawSql`, `rawQuery`
- `insertMany` (needs RETURNING for multiple keys)

**Note on `rawExecuteCount`:** persistent's `rawExecuteCount` goes through `stmtExecute`, so it always returns 0 in this backend (the actual affected row count is never read). This affects esqueleto's `deleteCount`, `updateCount`, and `insertSelectWithConflictCount`. The non-count variants (`delete`, `update`, `insertSelectWithConflict`) work correctly since they discard the return value.

### Lazy Reply Stream

All pipeline results are read through a single lazy reply stream, inspired by [Hedis](https://www.iankduncan.com/engineering/2026-02-17-archive-redis-pipelining).

```haskell
data PgConn = PgConn
    { ...
    , pgReplies :: !(IORef [LibPQ.Result])  -- lazy reply stream
    }
```

The stream is built at connection time with `unsafeInterleaveIO`:

```haskell
mkReplyStream :: PgConn -> IO [LibPQ.Result]
mkReplyStream pc = go
  where
    go = unsafeInterleaveIO $ do
        pgFlush pc                    -- flush send buffer
        ret <- readResultAndSep pc    -- read one result + NULL separator
        rest <- go                    -- next element (lazy thunk)
        return (ret : rest)
```

`pgRecvResult` pops using `head`/`tail` (not pattern matching) to keep the cons cell lazy:

```haskell
pgRecvResult :: PgConn -> IO LibPQ.Result
pgRecvResult pc = atomicModifyIORef (pgReplies pc) (\xs -> (tail xs, head xs))
```

`atomicModifyIORef` is lazy in the function result–  neither `head` nor `tail` is evaluated. The IO only fires when the caller forces the returned `LibPQ.Result`. This is the key property that enables automatic pipelining: multiple `pgRecvResult` calls accumulate unevaluated thunks, and the first force triggers a flush that sends all queued queries at once.

The ordering guarantee: each thunk N is created inside thunk N-1's `unsafeInterleaveIO` body, so results are always read in pipeline order regardless of which thunk the caller forces first.

### Drain Points

Results accumulate in the server's output buffer and are read at these drain points:

1. **`withStmt'` (any read operation)**–  calls `drainPending` before executing the read query.
2. **`connCommit`**–  drains all pending results plus the COMMIT result, verifying none failed.
3. **`connRollback`**–  drains everything to the sync point, ignoring errors.

### Transaction Lifecycle

```
connBegin:   sendQueryParams "BEGIN"  → increment pgPending (fire-and-forget)
             [user DML operations     → each increments pgPending]
             [user read operations    → each drains all pending first]
connCommit:  sendQueryParams "COMMIT" → pipelineSync
             → drain (N pending + 1 COMMIT) results
             → read PipelineSync marker
             → throw if any query failed
```

BEGIN is pipelined with the first user query–  zero extra round-trips for transaction setup.

### Examples

**100 deletes then select (fire-and-forget pipelining):**

```haskell
forM_ keys delete          -- 100x sendQueryParams, pgPending = 100
selectList [] []           -- drainPending (reads 100 results in one pass)
                           -- then sends SELECT and reads its result
```

Without pipelining: 101 round-trips. With pipelining: ~2 round-trips.

**100 gets (Hedis-style lazy pipelining):**

```haskell
results <- mapM get keys   -- 100x send SELECT (no flush, no read)
                           -- 100x pop lazy reply from stream
print results              -- forces first thunk → flushes ALL 100 queries
                           -- reads 100 results sequentially (already buffered)
```

Without pipelining: 100 round-trips. With pipelining: 1 flush + 100 sequential reads.
At 1ms/direction: **14ms** (pipeline) vs **280ms** (sequential)–  **20× faster**.

**100 inserts (pipelined RETURNING):**

```haskell
keys <- mapM insert recs   -- 100x send INSERT RETURNING (no flush, no read)
                           -- 100x pop lazy reply
evaluate (length keys)     -- forces first thunk → flushes ALL 100 queries
                           -- reads 100 keys sequentially
```

Without pipelining: 100 round-trips. With pipelining: 1 flush + 100 sequential reads.
At 5ms/direction: **41ms** (pipeline) vs **1.2s** (sequential)–  **29× faster**.

### Error Handling

**Mid-transaction error (during `drainPending`):**
1. All N pending results are drained and the counter is reset to 0.
2. Errors are collected; the first error is thrown.
3. The exception propagates to persistent, which calls `connRollback`.
4. Rollback sends ROLLBACK + sync → `drainToSync` consumes everything (including `PipelineAbort` status for queries after the failed one) → connection is clean.

**Commit-time error:**
1. All N+1 results (pending + COMMIT) are drained, sync marker is consumed.
2. Pipeline is fully clean before the error is thrown.
3. persistent calls `connRollback` → sends into an empty pipeline → clean drain.

**Pipeline abort state:** After a query error in pipeline mode, PostgreSQL marks subsequent queued queries with `PipelineAbort` status until the next sync point. The `drainNResults` helper handles this by collecting errors and skipping aborted results. The sync in commit/rollback resets the abort state.

## Binary Protocol

### Encoding (`Encoding.hs`)

`encodePersistValue` converts each `PersistValue` variant to a `(Oid, ByteString, Format)` triple using `postgresql-binary` encoders:

| PersistValue | PostgreSQL Type | OID |
|---|---|---|
| `PersistText` | text | 25 |
| `PersistInt64` | int8 | 20 |
| `PersistDouble` | float8 | 701 |
| `PersistBool` | bool | 16 |
| `PersistDay` | date | 1082 |
| `PersistUTCTime` | timestamptz | 1184 |
| `PersistByteString` | bytea | 17 |
| `PersistRational` | numeric | 1700 |
| `PersistNull` | –  | (Nothing) |
| `PersistArray` | typed array | inferred |
| `PersistList` | unknown (JSON text) | 0 |
| `PersistMap` | unknown (JSON text) | 0 |
| `DbSpecific`/`Escaped` | unknown (text format) | 0 |

`PersistArray` (used by the IN→ANY rewrite) infers the element type from the first non-null element and encodes as a native PostgreSQL array.

`PersistLiteral_ Unescaped` values are inlined into the SQL text before encoding (see SQL Rewriting below) and should never reach the encoder.

### Decoding (`Decoding.hs`)

`decodePersistValue` dispatches on the column OID to the appropriate `postgresql-binary` decoder. Covers scalar types, array types (bool[], int8[], text[], timestamptz[], etc.), JSON/JSONB, UUID (binary → hex text), money, interval, and more. Unknown OIDs fall back to `PersistLiteralEscaped` with raw bytes.

## Direct Decode/Encode Path

In addition to the `PersistValue`-based path, the backend supports a direct codec path that bypasses `PersistValue` entirely. See the [RFC](../RFC-direct-decode.md) for the full design rationale.

### Three layers

| Layer | Type | Scope | Purpose |
|-------|------|-------|---------|
| `FromRow` / `RowReader` | Backend-agnostic (in `persistent` core) | Row | Sequence field decoders across columns |
| `FieldDecode` / `FieldRunner` | Backend-agnostic (in `persistent` core) | Column | Prepare-once OID dispatch, per-row decode |
| `PgDecode` / `PgDecoder` | PostgreSQL-specific (`PgCodec.hs`) | Value | Compose inside arrays/composites |

### `PgRowEnv`–  the row environment

```haskell
data PgRowEnv = PgRowEnv
    { pgResult   :: !LibPQ.Result
    , pgRow      :: !LibPQ.Row
    , pgCols     :: !(V.Vector (LibPQ.Column, PgType))
    , pgRowCache :: !OidCache
    }
```

`FieldDecode PgRowEnv` instances inspect `pgCols` to select the right `postgresql-binary` decoder once per result set (via `prepareRow`), then read binary data from `pgResult`/`pgRow` on each row.

### Prepare-once execution

`FromRow` exposes `prepareRow` which calls `prepareField` for each column once, captures the `FieldRunner`s in a `RowDecoder` closure, and reuses them across all rows. The per-row loop calls only `runField`–  no OID dispatch, no vector lookup, no branching on column types.

### `PgParam`–  encoded parameters

```haskell
data PgParam
    = PgNull
    | PgValue {-# UNPACK #-} !LibPQ.Oid !ByteString !LibPQ.Format
```

Unpacked ADT replacing `Maybe (Oid, ByteString, Format)`. Converted to libpq's representation via `pgParamToLibPQ` at the send boundary.

### Value-level codecs (`PgCodec.hs`)

For compound types (arrays, composites), `PgDecode` / `PgEncode` compose through PostgreSQL's binary wire format:

```haskell
newtype PgDecoder a = PgDecoder { runPgDecoder :: OidCache -> PD.Value a }

class PgDecode a where pgDecoder :: PgDecoder a
class PgEncode a where pgEncoder :: PgEncoder a; pgTypeOid' :: OidCache -> Proxy a -> Word32
```

DSL: `pgValue`, `pgComposite`, `pgField`, `pgFieldNullable`, `pgArray`, `pgArrayNullable`. Generic `FieldDecode PgRowEnv [a]` instance via `PgDecode`.

### `SqlBackend` bridge (`DirectEntity`)

`SqlBackend` stores a `DirectQueryCap` that existentially hides `PgRowEnv`. Entities with `DirectEntity` instances use `Typeable`/`eqTypeRep` to recover the concrete env at query time. See `rawSqlDirectCompat` in `DirectRaw.hs`.

### `HasDirectQuery`–  concrete backend path

For code that retains the concrete backend type (e.g. `WriteBackend PostgreSQLBackend`), `HasDirectQuery` provides zero-overhead static dispatch with no `Typeable` involved. The `SqlBackend` bridge is only for code that flows through the erased `SqlBackend` type.

## SQL Rewriting

Three transformations happen between persistent's generated SQL and what gets sent to PostgreSQL:

### 1. Unescaped Literal Inlining (`inlineUnescaped`)

`PersistLiteral_ Unescaped` values are raw SQL fragments (e.g., `EXCLUDED."field_name"`). These can't be sent as bind parameters–  they're spliced directly into the SQL text, and removed from the parameter list.

### 2. IN → ANY Collapsing (`collapseInClauses`)

Rewrites:
- `IN (?,?,?)` → `= ANY(?)` with parameters collapsed into a single `PersistArray`
- `NOT IN (?,?,?)` → `<> ALL(?)` with the same collapsing

This reduces the number of bind parameters and lets PostgreSQL use its native array comparison operators. The rewriter is SQL-aware: it skips string literals, quoted identifiers, and comments.

### 3. Placeholder Rewriting (`rewritePlaceholders`)

Converts `?` placeholders to `$1, $2, ...` numbered parameters as required by libpq's `sendQueryParams`. `??` (persistent's column-expansion escape) becomes a literal `?`.

## Pipeline Helpers

Six internal functions implement the libpq pipeline result protocol:

| Function | Purpose |
|---|---|
| `drainOneResult` | Read one result + NULL separator, free it, return error if any |
| `readOneQueryResult` | Read one result + NULL separator, return it (caller frees), throw on error |
| `drainNResults` | Drain N results collecting errors, does NOT throw |
| `drainSyncResult` | Read PipelineSync result (no NULL separator after it) |
| `drainToSync` | Drain everything until PipelineSync, ignoring all errors |
| `drainPending` | Flush + drain all pending fire-and-forget results, throw if any failed |

## API Surface

### Drop-in replacement for `persistent-postgresql`

```haskell
createPostgresqlPipelinePool :: ConnectionString -> Int -> m (Pool SqlBackend)
withPostgresqlPipelinePool   :: ConnectionString -> Int -> (Pool SqlBackend -> m a) -> m a
withPostgresqlPipelineConn   :: ConnectionString -> (SqlBackend -> m a) -> m a

getPipelineConn :: backend -> Maybe PgConn
createRawPostgresqlPipelinePool :: ConnectionString -> Int -> m (Pool (RawPostgresqlPipeline SqlBackend))
```

All standard persistent operations (`insert`, `get`, `selectList`, `delete`, `update`, `upsert`, etc.) work transparently. No user code changes required.

### Direct decode/encode (zero-`PersistValue` path)

For code with the concrete backend type:

```haskell
rawQueryDirect :: (FromRow (Env backend) a, HasDirectQuery backend)
    => Text -> ParamBuilder (Param backend) -> ReaderT backend m (Acquire (ConduitM () a m ()))
rawSqlDirect  :: (FromRow (Env backend) a, HasDirectQuery backend)
    => Text -> ParamBuilder (Param backend) -> ReaderT backend m [a]
```

For code through `SqlBackend`:

```haskell
rawSqlDirectCompat :: (DirectEntity a)
    => Text -> [PersistValue] -> ReaderT SqlBackend m (Maybe [a])
```

Backend-specific codec modules:

```haskell
-- Re-exported from Database.Persist.Postgresql.Internal.DirectDecode
PgRowEnv (..)          -- row environment
compositeFieldDecode   -- one-liner for composite FieldDecode instances

-- Re-exported from Database.Persist.Postgresql.Internal.PgCodec
PgDecode (..), PgEncode (..)  -- value-level codec classes
pgValue, pgComposite, pgField, pgFieldNullable, pgArray, pgArrayNullable  -- decode DSL
pgConst, pgEncodeField, pgArrayEncoder, pgCompositeEncoder                -- encode DSL
```
