(ns graph-services.bolt
  (:require
   [graph-services.config                   :as cnf :refer [config]]
   [clojure.tools.logging                   :as log]
   [clojure.data.json                       :as json]
   [schema.core                             :as s])
  (:import
   [org.neo4j.driver.v1 Driver GraphDatabase AuthTokens Session StatementResult]))

(defn ^Driver make-driver []
  (GraphDatabase/driver "bolt://localhost:7687" (AuthTokens/basic "neo4j" "password")))

(defmacro with-conn [connvar & body]
  `(let [driver# (make-driver)]
     (with-open [~connvar (.session driver#)]
       ~@body)))

;; TODO: implement a record->map helper & use it from statement-result->vec

(defn statement-result->vec [^StatementResult sr]
  (loop [sr sr
         res []]
    (cond
      (.hasNext sr)
      (recur sr (conj res (.next sr)))

      :otherwise
      res)))

(comment
  (macroexpand
   '(with-conn conn
      (.run conn "MATCH (:User) RETURN count(*)")))

  (def res1
    (with-conn conn
      (->
       (.run conn "MATCH (:User) RETURN count(*)")
       statement-result->vec)))

  (.keys res1)
  ["count(*)"]

  (.next res1)
  

  (-> config deref :neo4j)

  

  )
