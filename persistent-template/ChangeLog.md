## Unreleased changes

## 2.7.3

* Update module documentation for `Database.Persist.TH` to better describe the purpose of the module [#968](https://github.com/yesodweb/persistent/pull/968)
* Support template-haskell-2.15 [#959](https://github.com/yesodweb/persistent/pull/959)

## 2.7.2

* Expose the knot tying logic of `parseReferences` so that users can build
  migrations from independently define entities at runtime [#932](https://github.com/yesodweb/persistent/pull/932)

## 2.7.1

* Add the `mkEntityDefList` function to work around [#902](https://github.com/yesodweb/persistent/issues/902). [#904](https://github.com/yesodweb/persistent/pull/904)

## 2.7.0

* Depends on `persistent-2.10.0` which provides the `OnlyOneUniqueKey` and `AtLeastOneUniqueKey` classes. Automatically generates instances for these classes based on how many unique keys the entity definition gets. This changes requires `UndecidableInstances` to be enabled on each module that generates entity definitions. [#885](https://github.com/yesodweb/persistent/pull/885)
* Removed deprecated `sqlOnlySettings`. Please use `sqlSettings` instead. [#894](https://github.com/yesodweb/persistent/pull/894)

## 2.6.0
* [persistent#846](https://github.com/yesodweb/persistent/pull/846): Improve error message when marshalling fails
* [persistent#826](https://github.com/yesodweb/persistent/pull/826): Change `Unique` derive `Show`

## 2.5.4

* [persistent#778](https://github.com/yesodweb/persistent/issues/778): Add `persistManyFileWith`.

## 2.5.3.1

* Slight improvement to the error message when a Persistent field can't be parsed from database results

## 2.5.3

* Exposed `parseReferences` to allow custom QuasiQuoters

## 2.5.2

* Fix incorrect `ToJSON`/`FromJSON` instance generation for generic
  backends

## 2.5.1.6

Allow non-null self-references in a list

## 2.5.1.4

* Allow composite Primary keys for tables that contain nullable fields.
* Support foreign keys to non-integer ids

## 2.5.1.3

* fix GHC 7.8 bug when a field name is "type"

## 2.5.1.2

* fix a bad Eq instance /= definition for Key when mpsGenetric=True

## 2.5.0.1

* workaround TH bug in GHC 7.10

## 2.5

* read/write typeclass split

## 2.1.6

* aeson 0.11
* transformers 0.5
## 2.1.4

support http-api-data for url serialization

## 2.1.3.3

By default explicitly use Int64 for foreign key references.
This avoids confusion on a 32 bit system.

## 2.1.3.1

Support foreign key references to composite primary keys

## 2.1.0.1

Support for monad-control 1.0
