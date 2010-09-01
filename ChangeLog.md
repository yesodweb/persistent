### Persistent 0.2.0 (August 29, 2010)

* SQL prepared statements are cached. This can lead to a huge speedup,
especially on SQLite.

* Automated SQL database migrations. This works by introspecting the database
and comparing it with the expected structure. There are both "safe" and
"unsafe" migration functions: the latter will never drop your data, whereas
the latter will. There is also support for getting a list of the proposed SQL
statements so that you can use an external tool, such as dbmigrations.

* Support for pagination in the select function; in particular, you can now
specify an offset and a limit.

* The select function now has an enumerator interface. A selectList function
is provided as well that returns a list of entities.

* Support for creating SQL foreign keys.

* A DeleteCascade typeclass and Template Haskell helpers to do a "deep delete"
of data.

* Upgraded the sqlite3 C library included to sqlite 3.7.0.1.

* Support for the In/NotIn filters.

* Allow execution of arbitrary SQL code. This is a bit under the surface, but
is necessary sometimes, such as when using the PostgreSQL full-text search.

### New in persistent 0.1.0

* Split up the PersistEntity typeclass into PersistBackend and PersistEntity.
This makes the TH generation code universal to all backends, and means backend
code doesn't require any TH wizardry.

* Added connection pool support.

* Replaced the Hamlet dependency with a blaze-html dependency.

* Attribute values available on entity as well as columns.

* You can have quoted attribute values, which allow multi-word attributes.

* Arbitrary SQL table and column name mapping using the "sql=name" attribute.
