(ns graph-services.scratch
  (:require
   [clojure.tools.logging                   :as log]
   [schema.core                             :as s]
   [clojurewerkz.neocons.rest               :as nr]
   [clojurewerkz.neocons.rest.nodes         :as nn]
   [clojurewerkz.neocons.rest.relationships :as nrl]
   [clojurewerkz.neocons.rest.cypher        :as cy]))


;; TODO: use object pooling
(def conn (nr/connect (graph-services.core/neo4j-url)))

(defn init-example-data []
  (cy/tquery conn "
          MATCH (n)
          OPTIONAL MATCH (n)-[r]-()
          DELETE n,r
")
  (let [ibc              (nn/create conn {:entity-type "Network"
                                          :name        "IBC"
                                          :relay-id    "IBC1901"})
        ibc-cs           (nn/create conn {:entity-type "Group"
                                          :name        "Customer Service"
                                          :relay-id    "IBC1901.customer-service"})
        ibc-cs-2way-pool (nn/create conn {:entity-type "TwoWayPool"
                                         :name        "Customer Service TwoWay Pool"
                                         :relay-id    "IBC1901.customer-service.two-way-pool"})
        ibc-mkting       (nn/create conn {:entity-type "Group"
                                          :name        "Marketing"
                                          :relay-id    "IBC1901.marketing"})
        rel1             (nrl/create conn ibc ibc-cs :group-in)
        rel2             (nrl/create conn ibc ibc-mkting :group-in)
        rel2             (nrl/create conn ibc-cs ibc-cs-2way-pool :routes-two-ways-to)]))

(defn all-nodes []
 (cy/tquery conn "MATCH node RETURN node "))

(comment

  (init-example-data)
  (all-nodes)




  )


















