(ns graph-services.scratch
  (:require
   [clojurewerkz.neocons.rest               :as nr]
   [clojurewerkz.neocons.rest.nodes         :as nn]
   [clojurewerkz.neocons.rest.relationships :as nrl]
   [clojurewerkz.neocons.rest.cypher        :as cy]
   [graph-services.config                   :as cnf :refer [config]]
   [clojure.tools.nrepl.server              :refer [start-server stop-server]]
   [cider.nrepl                             :refer [cider-nrepl-handler]]
   [clojure.tools.logging                   :as log]
   [clojure.data.json                       :as json]
   [schema.core                             :as s]))

(defn neo4j-db-url
  ([]
   (neo4j-db-url (:neo4j @config)))
  ([config]
   (format "%s://%s:%s@%s:%s/db/%s/"
           (:scheme   config "https")
           ;; TODO: uri escape the username & password
           (:username config "neo4j")
           (:password config)
           (:host     config "localhost")
           (:port     config 7474)
           (:dbname   config "data"))))

(defn connect!
  ([]
   (connect! (neo4j-db-url)))
  ([db-url]
   (nr/connect db-url)))


(comment
  (cnf/load-config!)


  (def conn (connect!))

  (def res1
    (cy/tquery conn "
 MATCH (flight:Flight)
 WHERE flight.distance > 500
RETURN flight"))

  (doall res1)

  ;; surprise!  nothing matches this query???
  ;; need to coerce flight.distance to an interger

  ;; Ok, so this must change the state of the database
  (def res2)
  (cy/tquery conn "
  MATCH (flight:Flight)
  SET flight.distance = toInteger (flight.distance)")

  (def res3
    (cy/tquery conn "
 MATCH (flight:Flight)
 WHERE flight.distance > 500
RETURN flight"))

  (count res3)
  ;; => 4402

  (take 3 res3)

  (-> res3 first keys)
  ("flight")

  (-> res3 first (get "flight"))

  (-> res3 first (get "flight") :data)

  (->>
   res3
   (map #(-> % (get "flight") :data))
   (take 3))

  
  


  )
