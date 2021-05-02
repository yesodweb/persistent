# TODOs remaining for improving QQ PR:

* MigrationOnly woes
    * Ok, so I have `getEntityFields` and then `getEntityFieldsDatabase`. Ths is
      annoying. The code should *just work* for `MigrationOnly` fields - for
      example, `fromPersistValues` possibly should *ignore* a potential
      `PersistNull` , instead of conditionally ignoring it.
* JSON decoding didn't work. Should have the test render an error message...
* Apparently calling 'persistFieldDef' on a composite primary key. Maybe I
  should defer that error to actual composite keys so natural single column keys
  work fine.
* Ugh. Okay, so `persistent` fully expects composite primary keys to Just Work
  with filter operators. This is bad. This means I can't just throw away the
  field defs. To be entirely proper, I could return a `NonEmpty FieldDef` from
  the `fieldDef` function. Or, like, `fieldDefMany`, and then `fieldDef =
  NEL.head . fieldDefMany`. But that's awful!

  In reality, I do want tos upport multi column fields, but I really don't want
  to include that support in this PR... it's already too big.
* RawSql Entity instance is broken?
* keyFromRecordM works on singleton case - apparently it isn't defined right.
* Cascades aren't working. This means the unbound cascades aren't properly being
  set on the behavior. This can be fixed in THSpec.

Test errors from `persistent-sqlite`:

```

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
