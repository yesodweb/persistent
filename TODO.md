# TODOs remaining for improving QQ PR:

Test errors from `persistent-sqlite`:

```
Failures:

  src/MigrationOnlyTest.hs:60:5:
  1) MigrationOnly field doesn't have the field in the Haskell entity
       uncaught exception: PersistException
       PersistMarshalError "TwoField: fromPersistValues failed on: [PersistInt64 5,PersistText \"hello\",PersistNull]"

  To rerun use: --match "/MigrationOnly field/doesn't have the field in the Haskell entity/"

  src/PersistentTest.hs:666:7:
  2) persistent.JsonEncoding decodes without an ID field
       expected: Just (Entity {entityKey = JsonEncodingKey {unJsonEncodingKey = "Bob"}, entityVal = JsonEncoding {jsonEncodingName = "Bob", jsonEncodingAge = 32}})
        but got: Nothing

  To rerun use: --match "/persistent/JsonEncoding/decodes without an ID field/"

  src/CompositeTest.hs:14:1:
  3) composite, primary keys, Insert
       uncaught exception: ErrorCall
       cannot get single FieldDef for Natural Key
       CallStack (from HasCallStack):
         error, called at src/CompositeTest.hs:14:1 in persistent-test-2.13.0.0-6vTeb9ClxA397Na0xrWicM:CompositeTest

  To rerun use: --match "/composite/primary keys/Insert/"

  src/CompositeTest.hs:14:1:
  4) composite, primary keys, Id field
       uncaught exception: ErrorCall
       cannot get single FieldDef for Natural Key
       CallStack (from HasCallStack):
         error, called at src/CompositeTest.hs:14:1 in persistent-test-2.13.0.0-6vTeb9ClxA397Na0xrWicM:CompositeTest

  To rerun use: --match "/composite/primary keys/Id field/"

  src/CompositeTest.hs:14:1:
  5) composite, primary keys, Filter by Id with 'not equal'
       uncaught exception: ErrorCall
       cannot get single FieldDef for Natural Key
       CallStack (from HasCallStack):
         error, called at src/CompositeTest.hs:14:1 in persistent-test-2.13.0.0-6vTeb9ClxA397Na0xrWicM:CompositeTest

  To rerun use: --match "/composite/primary keys/Filter by Id with 'not equal'/"

  src/CompositeTest.hs:14:1:
  6) composite, primary keys, Filter by Id with 'in'
       uncaught exception: ErrorCall
       cannot get single FieldDef for Natural Key
       CallStack (from HasCallStack):
         error, called at src/CompositeTest.hs:14:1 in persistent-test-2.13.0.0-6vTeb9ClxA397Na0xrWicM:CompositeTest

  To rerun use: --match "/composite/primary keys/Filter by Id with 'in'/"

  src/CompositeTest.hs:14:1:
  7) composite, primary keys, Filter by Id with 'not in'
       uncaught exception: ErrorCall
       cannot get single FieldDef for Natural Key
       CallStack (from HasCallStack):
         error, called at src/CompositeTest.hs:14:1 in persistent-test-2.13.0.0-6vTeb9ClxA397Na0xrWicM:CompositeTest

  To rerun use: --match "/composite/primary keys/Filter by Id with 'not in'/"

  src/CompositeTest.hs:14:1:
  8) composite, primary keys, Filter by Id with 'not in' with no data
       uncaught exception: ErrorCall
       cannot get single FieldDef for Natural Key
       CallStack (from HasCallStack):
         error, called at src/CompositeTest.hs:14:1 in persistent-test-2.13.0.0-6vTeb9ClxA397Na0xrWicM:CompositeTest

  To rerun use: --match "/composite/primary keys/Filter by Id with 'not in' with no data/"

  src/CompositeTest.hs:14:1:
  9) composite, primary keys, Insert Many to Many
       uncaught exception: ErrorCall
       cannot get single FieldDef for Natural Key
       CallStack (from HasCallStack):
         error, called at src/CompositeTest.hs:14:1 in persistent-test-2.13.0.0-6vTeb9ClxA397Na0xrWicM:CompositeTest

  To rerun use: --match "/composite/primary keys/Insert Many to Many/"

  src/CompositeTest.hs:233:5:
  10) composite, primary keys, RawSql Entity instance
       uncaught exception: ErrorCall
       TestParent: fromPersistValues failed on: [PersistText "p1"]

  To rerun use: --match "/composite/primary keys/RawSql Entity instance/"

  src/PrimaryTest.hs:51:7:
  11) primary key reference, keyFromRecordM, works on singleton case
       expected: Just (FooKey {unFooKey = "hello"})
        but got: Nothing

  To rerun use: --match "/primary key reference/keyFromRecordM/works on singleton case/"

  src/PrimaryTest.hs:57:7:
  12) primary key reference, keyFromRecordM, works on multiple fields
       expected: Just (CompositePrimaryKey {compositePrimaryKeyname = "hello", compositePrimaryKeyage = 31})
        but got: Nothing

  To rerun use: --match "/primary key reference/keyFromRecordM/works on multiple fields/"

  src/ForeignKey.hs:112:9:
  13) foreign keys options delete cascades
       expected: []
        but got: [Entity {entityKey = ChildKey {unChildKey = SqlBackendKey {unSqlBackendKey = 1}}, entityVal = Child {childPname = 1}}]

  To rerun use: --match "/foreign keys options/delete cascades/"

  src/ForeignKey.hs:118:9:
  14) foreign keys options update cascades
       expected: [2]
        but got: [1]

  To rerun use: --match "/foreign keys options/update cascades/"

  src/ForeignKey.hs:125:9:
  15) foreign keys options delete Composite cascades
       expected: []
        but got: [Entity {entityKey = ChildCompositeKey {unChildCompositeKey = SqlBackendKey {unSqlBackendKey = 1}}, entityVal = ChildComposite {childCompositePname = 1, childCompositePlastName = 2}}]

  To rerun use: --match "/foreign keys options/delete Composite cascades/"

  src/ForeignKey.hs:132:9:
  16) foreign keys options delete self referenced cascades
       expected: []
        but got: [Entity {entityKey = SelfReferencedKey {unSelfReferencedKey = 2}, entityVal = SelfReferenced {selfReferencedName = 2, selfReferencedPname = 1}}]

  To rerun use: --match "/foreign keys options/delete self referenced cascades/"

  src/ForeignKey.hs:150:9:
  17) foreign keys options delete cascades with explicit Reference
       expected: []
        but got: [Entity {entityKey = BKey {unBKey = SqlBackendKey {unSqlBackendKey = 1}}, entityVal = B {bBa = 1, bBb = 15}}]

  To rerun use: --match "/foreign keys options/delete cascades with explicit Reference/"

  src/ForeignKey.hs:181:9:
  18) foreign keys options deletes sets null with self reference
       expected: [Entity {entityKey = ChainKey {unChainKey = SqlBackendKey {unSqlBackendKey = 2}}, entityVal = Chain {chainName = 2, chainPrevious = Nothing}}]
        but got: [Entity {entityKey = ChainKey {unChainKey = SqlBackendKey {unSqlBackendKey = 2}}, entityVal = Chain {chainName = 2, chainPrevious = Just (ChainKey {unChainKey = SqlBackendKey {unSqlBackendKey = 1}})}}]

  To rerun use: --match "/foreign keys options/deletes sets null with self reference/"

  src/ForeignKey.hs:189:9:
  19) foreign keys options deletes cascades with self reference to the whole chain
       expected: []
        but got: [Entity {entityKey = Chain2Key {unChain2Key = SqlBackendKey {unSqlBackendKey = 2}}, entityVal = Chain2 {chain2Name = 2, chain2Previous = Just (Chain2Key {unChain2Key = SqlBackendKey {unSqlBackendKey = 1}})}},Entity {entityKey = Chain2Key {unChain2Key = SqlBackendKey {unSqlBackendKey = 3}}, entityVal = Chain2 {chain2Name = 3, chain2Previous = Just (Chain2Key {unChain2Key = SqlBackendKey {unSqlBackendKey = 2}})}}]
```
