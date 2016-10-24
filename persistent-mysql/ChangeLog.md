## 2.6

Compatibility for backend-specific upsert functionality.
A lucky contributor could add upsert to the MySQL backend now, i.e.:
INSERT ... ON DUPICATE ...

## 2.5

* changes for read/write typeclass split

## 2.3.0.1

Support usign default= for changing the id field type

## 2.3

* Distinguish between binary and non-binary strings in MySQL [#451](https://github.com/yesodweb/persistent/pull/451)
	* Previously all string columns (VARCHAR, TEXT, etc.) were being returned from Persistent as `PersistByteString`s (i.e. as binary data). Persistent now checks character set information to determine if the value should be returned as `PersistText` or `PersistByteString`.
	* This is a **breaking change** if your code is relying on a `PersistByteString` being returned for string-like MySQL values; persistent-mysql itself had several runtime errors that needed to be fixed because of this patch. High-level code dealing purely with `PersistEntities` should be unaffected.

## 2.2

* Update to persistent 2.2

## 2.1.3

* Added a `Show` instance for `MySQLConf`.

## 2.1.2.1

Documentation typo fix

## 2.1.2

Provide a `FromJSON` instance for `MySQLConf`.
