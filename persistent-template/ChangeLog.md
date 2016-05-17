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
