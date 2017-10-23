(ns graph-services.bolt
  (:require
   [graph-services.config                   :as cnf :refer [config]]
   [clojure.tools.logging                   :as log]
   [clojure.data.json                       :as json]
   [schema.core                             :as s])
  (:import
   [org.neo4j.driver.v1
    Driver GraphDatabase AuthTokens Session StatementResult
    Record]))

(defn ^Driver make-driver []
  (GraphDatabase/driver "bolt://localhost:7687" (AuthTokens/basic "neo4j" "password")))

(defmacro with-conn [connvar & body]
  `(let [driver# (make-driver)]
     (with-open [~connvar (.session driver#)]
       ~@body)))

(defn rec->map [^Record r]
  ;; should we keyword ize the keys?
  (zipmap (map keyword (.keys r))
          (.values r)))

(defn statement-result->vec [^StatementResult sr]
  (loop [sr sr
         res []]
    (cond
      (.hasNext sr)
      (recur sr (conj res (rec->map (.next sr))))

      :otherwise
      res)))

(comment
  (macroexpand
   '(with-conn conn
      (.run conn "MATCH (:User) RETURN count(*)")))

  (def res1
    (with-conn conn
      (->
       (.run conn "MATCH (:User) RETURN count(*) AS count")
       statement-result->vec)))

  ;; TODO: hepler that wraps up the above form into an exec-cypher!
  

  )
