# Changelog for persistent-mongoDB

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
