(ns web-service.session
  (:require
   [clojure.tools.logging :as log]
   [schema.core           :as s]))

(defonce ^{:dynamic true} session :this-should-be-a-map-in-an-atom)


(defn session-put! [k v]
  (swap! session assoc k v))

(defn session-get [k]
  (get @session k))

(defn session-get-in [ks]
  (get-in @session ks))

(defn session-del! [k]
  (swap! session dissoc k))

(defn session-contains? [k]
  (contains? @session k))

(defn session-merge! [m]
  (swap! session merge m))

(defn delete! []
  (reset! session (select-keys @session [:session-id :requests])))

(defn wrap-with-session-binding [h]
  (fn [r]
    (let [session1 (:session r {})
          session1 (assoc
                       session1
                     :requests (inc (or (:requests session1 0) 0)))]
      (binding [session (atom session1)]
        (let [resp (h r)]
          (when (map? resp)
            (assoc resp
              :session @session)))))))



