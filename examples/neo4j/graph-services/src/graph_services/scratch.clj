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

  
  
  (def res4
    (cy/tquery conn "
 MATCH (flight:Flight)
 WHERE flight.distance > 500
RETURN flight
 LIMIT 5"))

  res4

  (cy/tquery conn "match (n) detach delete n")
  (time
   (cy/tquery conn "
  LOAD CSV WITH HEADERS FROM \"https://raw.githubusercontent.com/neo4j-contrib/training/master/modeling/data/flights_initial.csv \" AS row
  MERGE (origin:Airport {code: row.Origin})
  MERGE (destination:Airport {code: row.Dest})
  WITH row.UniqueCarrier + row.FlightNum + \"_ \" + row.Year + \"- \" + row.Month + \"- \" + row.DayofMonth + \"_ \" + row.Origin + \"_ \" + row.Dest AS flightIdentifier, row
  MERGE (flight:Flight { id: flightIdentifier })
  ON CREATE SET flight.date = row.Year + \"- \" + row.Month + \"- \" + row.DayofMonth,
                flight.airline = row.UniqueCarrier, flight.number = row.FlightNum, flight.departure = row.CRSDepTime,
                flight.arrival = row.CRSArrTime, flight.distance = row.Distance, flight.cancelled = row.Cancelled
  MERGE (flight)-[:ORIGIN]->(origin)
  MERGE (flight)-[:DESTINATION]->(destination)"))

  ;; update the cancelled to a boolean
  (cy/tquery conn "
MATCH (flight:Flight)
  SET flight.cancelled = CASE WHEN flight.cancelled = \"1\" THEN true ELSE false END
")

  ;; then you can query cancelled as a bool
  (def res5
   (cy/tquery conn "
 MATCH (flight:Flight)
 WHERE flight.cancelled
RETURN flight"))

  (count res5)
  163

  (first res5)


  ;; import a larger dataset
  (cy/tquery conn "
USING PERIODIC COMMIT
LOAD CSV WITH HEADERS FROM \"https://raw.githubusercontent.com/neo4j-contrib/training/master/modeling/data/flights_50k.csv \" AS row
MERGE (origin:Airport {code: row.Origin})
MERGE (destination:Airport {code: row.Dest})
WITH row.UniqueCarrier + row.FlightNum + \"_ \" + row.Year + \"- \" + row.Month + \"- \" + row.DayofMonth + \"_ \" + row.Origin + \"_ \" + row.Dest AS flightIdentifier, row
MERGE (flight:Flight { id: flightIdentifier })
ON CREATE SET flight.date = row.Year + \"- \" + row.Month + \"- \" + row.DayofMonth,
              flight.airline = row.UniqueCarrier, flight.number = row.FlightNum, flight.departure = row.CRSDepTime,
              flight.arrival = row.CRSArrTime, flight.distance = row.Distance, flight.cancelled = row.Cancelled
MERGE (flight)-[:ORIGIN]->(origin)
MERGE (flight)-[:DESTINATION]->(destination)")

  ;; NB: still pretty quick

  (cy/tquery conn "MATCH (:Flight) RETURN count(*)")
  ;; => ({"count(*)" 49999})

  )
