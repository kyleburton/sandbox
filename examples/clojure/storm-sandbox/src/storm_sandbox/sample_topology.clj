(ns storm-sandbox.sample-topology
  (require
   [clojure.tools.logging    :as log]
   [org.apache.storm.clojure :as storm-clojure :refer [defspout spout emit-spout!
                                                       defbolt bolt fail! ack!]])
  (import
   [org.apache.storm.tuple Fields Values]))

(defspout rand-long-spout
  ["count" "randLong"]
  {}
  [conf context collector]
  (let [count     (java.util.concurrent.atomic.AtomicLong. 0)
        remaining (java.util.concurrent.atomic.AtomicLong. 100)
        rnd       (java.util.Random.)]
    (spout
     (nextTuple []
                (when (> (.decrementAndGet remaining) 0)
                  (Thread/sleep 100)
                  (let [rand-value (.nextLong rnd)
                        ctr        (.getAndIncrement count)
                        tuple      [ctr rand-value]]
                    (log/infof "rand-long-spout/nextTuple: I'm emitting a tuple=%s id=%s" tuple
                               ctr)
                    (emit-spout! collector tuple :id ctr))))
     (ack [id]
          (log/infof "tuple was processed successfully: %s" id)))))

(defbolt logging-bolt
  ["nothing"]
  {:prepare true}
  [conf context collector]
  (bolt
   (prepare [stormConf context collector]
            (log/infof "logging-bolt/prepare: stormConf=%s context=%s collector=%s"
                       stormConf context collector))
   (execute [tuple]
            (log/infof "logging-bolt/execute: I'm processing a tuple=%s" tuple)
            (ack! collector tuple))))

(defn make-topology-config []
  {:name   "sample-topology-1"
   :spouts [{:name  "rand-long-spout"
             :spout rand-long-spout}]
   :bolts  [{:name   "logging-bolt"
             :bolt   logging-bolt
             :inputs ["rand-long-spout"]}]})
