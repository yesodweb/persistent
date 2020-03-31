# Changelog for persistent-mongoDB

## 2.10.0.0

* Fix `ninList` filter operator [#1058](https://github.com/yesodweb/persistent/pull/1058)

## 2.9.0.2

* Compatibility with latest mongoDB [#1012](https://github.com/yesodweb/persistent/pull/1012)

## 2.9.0.1

* Compatibility with latest persistent-template for test suite [#1002](https://github.com/yesodweb/persistent/pull/1002/files)

## 2.9.0

* Removed deprecated `entityToDocument`. Please use `recordToDocument` instead. [#894](https://github.com/yesodweb/persistent/pull/894)
* Removed deprecated `multiBsonEq`. Please use `anyBsonEq` instead. [#894](https://github.com/yesodweb/persistent/pull/894)
* Use `portID` from `mongoDB` instead of `network`. [#946](https://github.com/yesodweb/persistent/pull/946)

## 2.8.0

* Switch from `MonadBaseControl` to `MonadUnliftIO`

## 2.6.0

* Fix upsert behavior [#613](https://github.com/yesodweb/persistent/issues/613)
* Relax bounds for http-api-data

## 2.5

* changes for read/write typeclass split

## 2.1.4

* support http-api-data for url serialization

## 2.1.3

* Add list filtering functions `inList` and `ninList`

## 2.1.2

* Add `nestAnyEq` filter function
* Add `FromJSON` instance for `MongoConf`
