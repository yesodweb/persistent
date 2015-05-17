## 2.1.6

add showMigration function

## 2.1.5

Add `dbIdColumnsEsc` to Sql.Utils.
Used in persistent-postgresql 2.1.5.2

## 2.1.4

* Fix getBy with a primary key. #342

## 2.1.3

* Break self-referencing cycles in the entity declarations

## 2.1.2

* Error with `Double`s without a decimal part [#378](https://github.com/yesodweb/persistent/issues/378)
* `runSqlPool` does not perform timeout checks.

## 2.1.1.6

* One extra feature for #939: use `logDebugN` instead

## 2.1.1.5

* Better SQL logging [Yesod issue #939](https://github.com/yesodweb/yesod/issues/939)

## 2.1.1.3

Parse UTCTime in 8601 format [#339](https://github.com/yesodweb/persistent/issues/339)

## 2.1.1.1

Support for monad-control 1.0
