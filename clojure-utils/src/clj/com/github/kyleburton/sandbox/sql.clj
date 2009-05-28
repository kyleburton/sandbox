(ns com.github.kyleburton.sandbox.sql
  (use [clojure.contrib.sql :as sql]
       [com.github.kyleburton.sandbox.utils :as kutils]))


(defn- range-sql [end]
    (range 1 (+ 1 end)))


(defn db-schemas
  "Returns a list of the schema names in the database."
  [db]
  (with-connection db
    (let [schemas (.getSchemas (.getMetaData (connection)))]
      (loop [has-next (.next schemas)
             res []]
        (if has-next
          (let [schema  (.getString schemas 1)]
            (recur (.next schemas) 
                   (conj res schema)))
          res)))))

(defn schema-tables
  "Returns a list of maps describing the tables in the database.  The
  maps include: :catalog, :schema, :name, :type and :remarks as per
  the JDBC spec."  
  [db schema]
  (with-connection db
    (let [db-meta (.getMetaData (connection))
          tables (.getTables db-meta nil schema "%" nil)]
      (loop [has-next (.next tables)
             res []]
        (if has-next
          (let [table {:catalog  (.getString tables  1)
                       :schema   (.getString tables  2)
                       :name     (.getString tables  3)
                       :type     (.getString tables  4)
                       :remakrs  (.getString tables  5)}]
            (recur (.next tables)
                   (conj res table)))
          res)))))

(defn describe-table
  "Returns a list of column descriptions (maps) for the table.  The
   maps
   contain: :name, :catalog, :display-zie, :type, :precision, :scale,
   :is-auto-increment, :is-case-sensitive, :is-currency,
   :is-definitely-writable, :is-nullable, :is-read-only,
   :is-searchable, :is-signed, :is-writable."
  [db table-name]
  (with-connection db
    (let [ps (.prepareStatement (connection) (format "SELECT * FROM %s WHERE 0 = 1" table-name))
          rs (.executeQuery ps)
          rs-meta (.getMetaData rs)]
      (loop [[idx & idxs] (range-sql (.getColumnCount rs-meta))
             res []]
        (if idx
          (recur idxs (conj res {:name                   (.getColumnName rs-meta idx)
                                 :catalog                (.getCatalogName rs-meta idx)
                                 :display-zie            (.getColumnDisplaySize rs-meta idx)
                                 :type                   (.getColumnType rs-meta idx)
                                 :precision              (.getPrecision rs-meta idx)
                                 :scale                  (.getScale rs-meta idx)
                                 :is-auto-increment      (.isAutoIncrement rs-meta idx)
                                 :is-case-sensitive      (.isCaseSensitive rs-meta idx)
                                 :is-currency            (.isCurrency rs-meta idx)
                                 :is-definitely-writable (.isDefinitelyWritable rs-meta idx)
                                 :is-nullable            (.isNullable rs-meta idx)
                                 :is-read-only           (.isReadOnly rs-meta idx)
                                 :is-searchable          (.isSearchable rs-meta idx)
                                 :is-signed              (.isSigned rs-meta idx)
                                 :is-writable            (.isWritable rs-meta idx)}))
          res)))))


(defn rs->record [rs]
  (loop [[idx & idxs] (range-sql (.getColumnCount (.getMetaData rs)))
         res []]
    (if idx
      (recur idxs
             (conj res (.getString rs idx)))
      res)))

(defn rs->map [rs]
  (loop [meta (.getMetaData rs)
         [idx & idxs] (range-sql (.getColumnCount meta))
         res {}]
    (if idx
      (recur meta
             idxs
             (assoc res (.getColumnName meta idx) (.getString rs idx)))
      res)))

;; TODO: support bind variables...
(defn sql->records [db sql]
  (with-connection db
    (let [ps (.prepareStatement (connection) sql)
          rs (.executeQuery ps)]
      (loop [has-next (.next rs)
             res []]
        (if has-next
          (let [rec (rs->record rs)]
            (recur (.next rs) 
                   (conj res rec)))
          res)))))

(defn sql->maps [db sql]
  (with-connection db
    (let [ps (.prepareStatement (connection) sql)
          rs (.executeQuery ps)]
      (loop [has-next (.next rs)
             res []]
        (if has-next
          (let [rec (rs->map rs)]
            (recur (.next rs) 
                   (conj res rec)))
          res)))))

(defmacro do-rs-rows
  [db sql var & body]
  `(with-connection ~db
     (let [ps# (.prepareStatement (connection) ~sql)
           rs# (.executeQuery ps#)
           fn# (fn [~var] ~@body)]
       (loop [has-next# (.next rs#)]
         (if has-next#
           (do
             (fn# (rs->record rs#))
             (recur (.next rs#))))))))

(defmacro do-rs-maps [db sql var & body]
  `(with-connection ~db
     (let [ps# (.prepareStatement (connection) ~sql)
           rs# (.executeQuery ps#)
           fn# (fn [~var] ~@body)]
       (loop [has-next# (.next rs#)]
         (if has-next#
           (do
             (fn# (rs->map rs#))
             (recur (.next rs#))))))))

