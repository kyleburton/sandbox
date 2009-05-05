(ns com.github.kyleburton.sandbox.mdb
  (:import (com.healthmarketscience.jackcess 
            Database TableBuilder
            ColumnBuilder DataType Table)
           (java.io File))
  (:require [com.github.kyleburton.sandbox.utils :as kutils]
            [clojure.contrib.duck-streams :as ds]
            [clojure.contrib.str-utils :as str]))

(defn new-col [name type]
 (let [cb (ColumnBuilder. name)]
   (.setSQLType cb (.getSQLType type))
   (.toColumn cb)))

(defmacro with-database [[var location] & body]
  `(let [location# (ds/file (str ~location))
         exists# (.exists location#)]
     (with-open [~var (if exists#
                        (Database/open location#)
                        (Database/create location#))]
       (let [res# (do ~@body)]
         (.flush ~var)
         res#))))

(defn- ->safe-name [thing]
  (.replaceAll
   (cond (keyword? thing)
         (.substring (str thing) 1)
         (string? thing)
         thing
         true
         (str thing))
   "[^a-zA-Z0-9]" "_"))

(defn create-table [#^Database db #^String table-name & cols]
  (let [builder (TableBuilder. table-name)]
    (loop [[[name type] & cols] (partition 2 cols)]
      (if name
        (do
          (.addColumn builder (new-col (->safe-name name) type))
          (recur cols))))
    (.toTable builder db)))

(defn create-or-get-table [#^Database db #^String table-name & cols]
  (if (.contains (.getTableNames db)
                 table-name)
    (.getTable db table-name)
    (apply create-table db table-name cols)))


;; (kutils/doc-class
;;  (with-database [db (kutils/$HOME "test.mdb")]
;;    (.getTableNames db)))


(defn insert-row [tbl & fields]
  (.addRow tbl (to-array fields)))

;; (class  (with-database [db (kutils/$HOME "test.mdb")] db))

(defn row->seq [cols row]
  (map #(get row (.getName %)) cols))

(defn for-each-row 
  ([#^Table tbl fn]
     (let [cols (.getColumns tbl)]
       (.reset tbl)
       (loop [row (.getNextRow tbl)]
         (if row
           (do
             (fn (row->seq cols row))
             (recur (.getNextRow tbl)))))))
  ([#^Database db #^String tbl fn]
     (for-each-row (.getTable db tbl) fn)))

(defn table->tab-file
  [#^String mdb-file #^String table-name #^String tab-file]
  (with-database [db mdb-file]
    (with-open [out (ds/writer tab-file)]
      (binding [*out* out]
        (for-each-row 
         db 
         table-name
         (fn [row]
           (println (str/str-join "\t" row))))))))


(defn- ->map [m]
  (reduce (fn [r k]
            (assoc r (keyword (.toLowerCase k)) (.get m k)))
          {}
          (keys m)))

;; (with-database [db (kutils/$HOME "test.mdb")]
;;    (prn (format "tables: %s" (.getTableNames db)))
;;    (let [tbl (create-or-get-table
;;               db 
;;               "TABLE_A"
;;               :id    DataType/INT
;;               :fname DataType/TEXT
;;               :lname DataType/TEXT
;;               :addr1 DataType/TEXT
;;               :addr2 DataType/TEXT
;;               :city  DataType/TEXT
;;               :state DataType/TEXT
;;               :zip   DataType/TEXT)]
;;      (insert-row tbl 1 "Kyle" "Burton" "2700 Horizon Drive" "STE 200" "King of Prussia" "PA" "19401")
;;      (insert-row tbl 1 "Kyle" "Burton" "625 W. Rdige Pike" "STE 400" "Conshohocken" "PA" "19401")
;;      (insert-row tbl 1 "Kyle" "Burton" "123 Main St" ""  "Wayne" "PA" "19401")))

(defn mdb->tables [mdb]
  (with-database [db mdb]
    (seq (.getTableNames db))))

;; (mdb->tables (kutils/$HOME "test.mdb"))


;; (table->tab-file
;;  (kutils/$HOME "test.mdb")
;;  "TABLE_A"
;;  (kutils/$HOME "table_a.tab"))

(defn tab-file->table [mdb table file]
  (with-database [db mdb]
    (.importFile db table (ds/file file) "\t")))

;; (tab-file->table (kutils/$HOME "test.mdb")
;;                  "TABLE_B"
;;                  (kutils/$HOME "table_a.tab"))

(defn display-table [mdb tbl]
  (with-database [db mdb]
    (.display (.getTable db tbl))))

;; (print (display-table (kutils/$HOME "test.mdb") "TABLE_A"))

;; (with-database [db (kutils/$HOME "test.mdb")]
;;    (prn (format "tables: %s" (.getTableNames db)))
;;    (let [tbl (.getTable db "TABLE_A")]
;;      (prn (format "columns[%s]: %s"
;;                   (.getName tbl)
;;                   (map #(.getName %) (seq (.getColumns tbl)))))
;;      (do
;;        (.reset tbl)
;;        (loop [row (.getNextRow tbl)]
;;          (if row
;;            (do
;;              (prn (format "row: %s" row))
;;              ;(prn (format "     %s" (->map row)))
;;              ;(prn (format "     id: %s" (:id (->map row))))
;;              (recur (.getNextRow tbl))))))))

(defmulti truncate-table (fn [& args] (map class args)))

(defmethod truncate-table [String String] [mdb table]
  (prn "S,S; opening db and delegating")
  (with-database [db mdb]
    (truncate-table db table)))

(defmethod truncate-table [File String] [mdb table]
  (prn "F,S; opening db and delegating")
  (with-database [db mdb]
    (truncate-table db table)))

(defmethod truncate-table [Database String] [db table]
  (prn "D,S; getting table and delegating")
  (truncate-table db (.getTable db table)))

;; probably faster to drop/create the table...but I don't see how to
;; delete a table in the api?  deleteCurrentRow doesn't seem to
;; work...
(defmethod truncate-table  [Database Table] [db table]
  (prn "D,T; rubber meets the road")
  (.reset table)
  (loop [row (.getNextRow table)]
    (prn "in loop, row:" row)
    (if row
      (do
        (prn "deleting row: " row)
        (.deleteCurrentRow table)
        (recur (.getNextRow table))))))


;; (mdb->tables (kutils/$HOME "tmp" "test.mdb"))
;; (truncate-table (kutils/$HOME "tmp" "test.mdb") "TABLE_B")

;; (print (display-table (kutils/$HOME "test.mdb") "TABLE_A"))
;; (print (display-table (kutils/$HOME "test.mdb") "TABLE_B"))

;; (with-database [db (kutils/$HOME "test.mdb")]
;;   (for-each-row db "TABLE_B" 
;;                 (fn [row] (prn "row=" row))))

