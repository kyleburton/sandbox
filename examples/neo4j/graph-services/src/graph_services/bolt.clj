(ns graph-services.bolt
  (:require
   [graph-services.config                   :as cnf :refer [config]]
   [clojure.tools.logging                   :as log]
   [clojure.data.json                       :as json]
   [schema.core                             :as s])
  (:import
   [org.neo4j.driver.v1 Driver GraphDatabase AuthTokens Session StatementResult]))

(defn make-driver []
  (GraphDatabase/driver "bolt://localhost:7687" (AuthTokens/basic "neo4j" "password")))

(defmacro with-conn [connvar & body]
  `(let [driver# (make-driver)]
     (with-open [~connvar (.session driver#)]
       ~@body)))

(defn statement-result->vec [sr]
  ;; call .next / .hasNext in a loop/recur, accum to a vector and return it
  :finish-this)

(comment
  (macroexpand
   '(with-conn conn
      (.run conn "MATCH (:User) RETURN count(*)")))

  (def res1
   (with-conn conn
     (.run conn "MATCH (:User) RETURN count(*)")))

  (.keys res1)
  ["count(*)"]

  (.next res1)
  

  (-> config deref :neo4j)

  

  )
