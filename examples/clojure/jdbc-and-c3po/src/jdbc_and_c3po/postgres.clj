(ns jdbc-and-c3po.postgres
  (:require
   [clojure.tools.logging :as log]
   [clojure.java.jdbc     :as jdbc]
   [clj-time.core         :as time]
   [clojure.java.jdbc     :as jdbc]
   [schema.core           :as s])
  (:import com.mchange.v2.c3p0.ComboPooledDataSource
           java.net.URI))

;; https://devcenter.heroku.com/articles/database-connection-pooling-with-clojure

;; jdbc:postgresql://host:port/database?param1=value1&param2=value2&
(defonce db (atom nil))

(defrecord DataSource [datasource]
  java.io.Closeable
  (close [_]
    (.close datasource)))

(def DataSourceConfig {(s/required-key :driver-class) s/Str
                       (s/required-key :jdbc-url)     s/Str
                       (s/required-key :username)     s/Str
                       (s/required-key :password)     s/Str})
(s/defn new-data-source [params :- DataSourceConfig]
  (let [ds (doto
               (ComboPooledDataSource.)
             (.setDriverClass (:driver-class params))
             (.setJdbcUrl     (:jdbc-url params))
             (.setUser        (:username params))
             (.setPassword    (:password params)))]
    ds))

(defn load-config []
  (let [lines (-> "pg.env" slurp
                  (.split "\n")
                  vec)
        lines (filter #(not (.startsWith % "#")) lines)
        pairs (mapv #(vec (.split % "=" 2)) lines)]
    (reduce
     (fn [acc [k v]]
       (assoc acc
              (-> k (.replaceAll "DOCKER_PG_" "") .toLowerCase keyword)
              (.replaceAll v "(^\"|\"$)" "")))
     {}
     pairs)))

(def DBInfo {(s/required-key :hostname) s/Str
             (s/required-key :username) s/Str
             (s/required-key :password) s/Str
             (s/required-key :dbname)   s/Str
             (s/required-key :port)     (s/either s/Num s/Str)})

(s/defn init! [db-info :- DBInfo]
  (reset! db {:datasource
              (new-data-source {:driver-class "org.postgresql.Driver"
                                :jdbc-url     (format "jdbc:postgresql://%s:%s/%s"
                                                      (:hostname db-info "localhost")
                                                      (:port db-info)
                                                      (:dbname db-info "postgres"))
                                :username     (:username db-info)
                                :password     (:password db-info)})}))

(comment
  (do
    (defonce config (load-config))
    (init! {:hostname (config :hostname "localhost")
            :username (config :username "postgres")
            :password (config :password)
            :dbname   (config :dbname   "postgres")
            :port     (config :port 15432)}))
  
  (def tables-like-user
    (jdbc/query
     @db
     ["select * from information_schema.columns WHERE table_name ilike ?" (str "%user%")]))


  (first tables-like-user)

  (mapv #(mapv % [:table_schema :table_name :column_name])
        tables-like-user)

  (reduce
   (fn [acc row]
     (update-in
      acc
      [(keyword (:table_schema row)) (keyword (:table_name row))]
      conj (:column_name row)))
   {}
   tables-like-user)


  (def rs
    (jdbc/query
     @db
     ["select now()"]))


  (.close (-> db deref :datasource))

  )


