(ns graph-services.scratch
  (:require
   [clojurewerkz.neocons.rest       :as nr]
   [clojurewerkz.neocons.rest.nodes :as nn]
   [graph-services.config           :as cnf :refer [config]]
   [clojure.tools.nrepl.server      :refer [start-server stop-server]]
   [cider.nrepl                     :refer [cider-nrepl-handler]]
   [clojure.tools.logging           :as log]
   [clojure.data.json               :as json]
   [schema.core                     :as s]))

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

  

)
