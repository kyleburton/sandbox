(ns jdbc-and-c3po.sqlite
  (:require
   [clojure.tools.logging :as log]
   [clojure.java.jdbc     :as jdbc]
   [clj-time.core         :as time]))

;; https://github.com/ogrim/clojure-sqlite-example

;; (-> (time/now) str)

(def testdata
  {:date  (time/now)
   :url   "https://news.ycombinator.com"
   :title "SQLite Example"
   :body  "Example using SQLite with Clojure"})

(def db
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "test-database.db"})

(defn create-db []
  (try (jdbc/db-do-commands
        db
        (jdbc/create-table-ddl
         :news
         [[:date :text]
          [:url :text]
          [:title :text]
          [:body :text]]))
       (catch Exception e
         (log/infof e "Error creating table: %s" e)
         (throw e))))

(defn select-all-from-news []
  (jdbc/query db "select * from news"))

(defn run []
  (create-db)
  (jdbc/insert! db :news testdata)
  (let [recs (select-all-from-news)]
    (log/infof "keys from news: %s" (keys (first recs)))
    (log/infof "body of first record: %s" (:body (first recs)))))

(comment

  (run)

  )
