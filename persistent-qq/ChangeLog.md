# Changelog for persistent-qq

## 2.12.0.1

* Support GHC 9. [#1265](https://github.com/yesodweb/persistent/pull/1265)
* Clarify lower bounds on `persistent` for the test suite. [#1274](https://github.com/yesodweb/persistent/pull/1274)

## 2.12.0.0

* Decomposed `HaskellName` into `ConstraintNameHS`, `EntityNameHS`, `FieldNameHS`. Decomposed `DBName` into `ConstraintNameDB`, `EntityNameDB`, `FieldNameDB` respectively. [#1174](https://github.com/yesodweb/persistent/pull/1174)

## 2.9.2.1

* Support `persistent-2.11` in the test suite [#1170](https://github.com/yesodweb/persistent/pull/1170)

## 2.9.2

* Add interpolation support for multirow VALUES syntax (`*{rows}`) [#1111](https://github.com/yesodweb/persistent/pull/1111)

## 2.9.1.1

* Compatibility with latest persistent-template for test suite [#1002](https://github.com/yesodweb/persistent/pull/1002/files)

## 2.9.1

* Added support for list of values in `sqlQQ`. [#819](https://github.com/yesodweb/persistent/pull/819)

## 2.9.0

* Initial release, code separated from `persistent`
