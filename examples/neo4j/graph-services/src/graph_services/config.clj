(ns graph-services.config
  (:require
   [clojure.tools.nrepl.server :refer [start-server stop-server]]
   [cider.nrepl                :refer [cider-nrepl-handler]]
   [clojure.tools.logging      :as log]
   [clojure.data.json          :as json]
   [schema.core                :as s]))


(defonce config (atom {}))

(defn load-config! []
  (let [conf (-> "Resources/server-config.json" slurp (json/read-str :key-fn keyword))]
    (reset! config (merge @config conf))))

(defn init! []
  (load-config!))

(comment
  (init!)

)
