## 2.1.4

* Add log support to persistent-sqlite [#381](https://github.com/yesodweb/persistent/pull/381)

## 2.1.3

* Added a `Show` instance for `SqliteConf`.
* Use `SqliteException` instead of calling `fail` [#364](https://github.com/yesodweb/persistent/issues/364)

## 2.1.2

* Turn on write-ahead log [#363](https://github.com/yesodweb/persistent/issues/363)
    * Prepending `WAL=off ` to your connection string will recover the previous behavior.

## 2.1.1.1

Fix rendering of `UTCTime` to match SQLite requirements (see [issue
#328](https://github.com/yesodweb/persistent/issues/328#issuecomment-65887577)).

## 2.1.1

Provide a `FromJSON` instance for `SqliteConf`.
