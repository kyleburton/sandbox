(ns storm-sandbox.core
  (require
   [clojure.tools.logging :as log]
   [storm-sandbox.sample-topology :as sample-topology]
   [org.apache.storm.clojure :as storm-clojure :refer [defbolt]])
  (import
   [org.apache.storm LocalCluster Config]
   [org.apache.storm.generated StormTopology]
   [org.apache.storm.tuple Fields Values]
   [org.apache.storm.topology TopologyBuilder]
   [org.apache.storm.topology.base BaseBasicBolt]
   [org.apache.storm LocalCluster]))

(def config (atom {:nrepl {:bind "127.0.0.1"
                           :port 4124}}))

(def repl-server (atom nil))

;; https://github.com/xetorthio/getting-started-with-storm/blob/master/ch02GettingStarted.asc

;; DONE: write a main
;; DONE: start a cider nrepl
;; TODO: get a standalone topology going
;; TODO: some periodic spout of data into the topology
;; TODO: multiple bolts processing data (go off of one of the examples)
;; TODO: bolt that stores data off somewhere (logfile? database?)
;; TODO[optional]: rabbitmq consumer?
;; TODO[optional]: join to something like redis?

(defn create-topology [topology-spec]
  (when-not (contains? topology-spec :name)
    (throw (RuntimeException. "Error: topology-spec must have a name!")))
  (let [builder      (TopologyBuilder.)
        storm-config (Config.)]
    (doseq [spout-spec (topology-spec :spouts)]
      (.setSpout builder
                 (:name spout-spec)
                 (:spout spout-spec)))
    (doseq [bolt-spec (topology-spec :bolts)]
      (let [bldr (.setBolt builder
                           (:name bolt-spec)
                           (:bolt bolt-spec))]
        (.shuffleGrouping bldr (-> bolt-spec :inputs first))))
    (doseq [[k v] (:storm-config topology-spec)]
      (.put storm-config k v))
    
    {:spec         topology-spec
     :storm-config storm-config
     :topology     (.createTopology builder)}))


(def local-cluster (atom nil))

(defn create-and-start! []
  (let [topology       (create-topology (sample-topology/make-topology-config))
        cluster        (LocalCluster.)
        topology-name  (-> topology :spec :name)
        storm-config   (-> topology :storm-config)
        built-topology (-> topology :topology)]
    (reset! local-cluster {:cluster       cluster
                           :topology-name topology-name
                           :storm-config  storm-config
                           :topology      built-topology})
    (.submitTopology
     cluster
     topology-name
     storm-config
     built-topology)))

(defn -main
  "Main: start up the local development topology"
  [& args]
  (com.github.kyleburton.Repl/launchCiderNRepl
   (-> config deref :nrepl :bind)
   (-> config deref :nrepl :port))
  (log/infof "TODO: [optional] launch topology in local mode"))


(comment
  (-> local-cluster deref :cluster .shutdown)
  (create-and-start!)

  )
