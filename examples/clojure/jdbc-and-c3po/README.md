# jdbc-and-c3po

This project shows how to use (`clojure.java.jdbc`)[https://github.com/clojure/java.jdbc] connected to SQLite, PostgreSQL and MySQL using (`C3P0`)[https://github.com/clojure/java.jdbc] database connection pooling.  NB: the sqlite example does not use connection pooling.

## Usage

To run the embedded Clojure [NREPL](https://github.com/clojure/java.jdbc), execute:

```
$ lein run
```

## Examples

* [`/src/jdbc_and_c3po/sqlite.clj`](/examples/clojure/jdbc-and-c3po/src/jdbc_and_c3po/sqlite.clj)
* [`/src/jdbc_and_c3po/postgres.clj`](/examples/clojure/jdbc-and-c3po/src/jdbc_and_c3po/postgres.clj)
* [`/src/jdbc_and_c3po/mysql.clj`](/examples/clojure/jdbc-and-c3po/src/jdbc_and_c3po/mysql.clj)

You can then connec to the repl, for example from [CIDER](https://github.com/clojure/java.jdbc).

## License

Copyright © 2017 Kyle R. Burton

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
