Database configuration may change the semantics of your database in important ways; this page lists options likely to affect Persistent users.

## MySQL

### Strict Mode

By default, MySQL will truncate too-long values, which can corrupt data (by e.g. truncating a binary file) or cause [unexpected behavior](https://github.com/yesodweb/persistent/issues/122). If you'd like to have MySQL raise an error instead, enable [strict mode](https://dev.mysql.com/doc/refman/5.6/en/sql-mode.html#sql-mode-strict). You can enable this by editing your `my.cnf` file:

```
sql_mode="STRICT_ALL_TABLES"
```

or by setting the SQL mode from the MySQL console:

```
SET GLOBAL sql_mode = 'STRICT_ALL_TABLES';
```

Note that strict mode causes slight changes in behavior; see the MySQL docs link above for details.

## SQLite

### Enable foreign key constraints

Foreign key checks are not enabled by default in SQLite. [This wiki page](https://github.com/yesodweb/yesod/wiki/Activate-foreign-key-checking-in-Sqlite) explains how to enable foreign key checks when using Persistent/Yesod.

### URI Filenames

Sqlite supports URI syntax with the advantage of additional configuration options via query parameters (cf. [Sqlite docs on URI filenames](https://www.sqlite.org/uri.html)).

* The scheme of the URI must be "file:". Any other scheme results in the input being treated as an ordinary filename.
* The authority may be omitted, may be blank, or may be "localhost". Any other authority results in an error. 
* The path is optional if the authority is present. If the authority is omitted then the path is required.

If no further query parameter is provided, the database is opened in the mode "read-write" and "create file" (e.g. create new file if it does not yet exist), the default behavior.

The mode query parameter determines if the new database is opened read-only, read-write, read-write and created if it does not exist, or that the database is a pure in-memory database that never interacts with disk, respectively.

mode=ro
mode=rw
mode=rwc
mode=memory

#### Connection string example

* Conventional style: "sqlite3.db"
* URI filename style, relative path, read-only: "file:sqlite3.db?mode=ro"