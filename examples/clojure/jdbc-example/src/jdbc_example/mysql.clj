(ns jdbc-example.mysql
  (:require
   [clojure.tools.logging :as log]
   [clojure.data.json     :as json]
   [clojure.data.csv      :as csv]
   [clojure.java.jdbc     :as sql]))


(comment
  (def mysql-db
    {:dbtype  "mysql"
     :dbname   "mysql"
     :user     "root"
     :password "password"})
  
  (sql/query mysql-db
             ["select database()"])

  ;; => ({:database() "mysql"})


  (->>
   (sql/query mysql-db
              ["select * from information_schema.tables"])
   (mapv (fn [row]
           (mapv row [:table_schema :table_name]))))

  (->>
   (sql/query mysql-db
              ["select * from information_schema.tables"])
   count)
  ;; => 299

  (sql/query mysql-db
             ["SELECT SCHEMA_NAME AS `Database` FROM INFORMATION_SCHEMA.SCHEMATA"])
  ;; ({:database "mysql"}
  ;;  {:database "information_schema"}
  ;;  {:database "performance_schema"}
  ;;  {:database "sys"})
  

  
  )
