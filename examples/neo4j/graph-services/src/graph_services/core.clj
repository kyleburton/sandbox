(ns graph-services.core
  (:require
   [graph-services.config      :as cnf]
   [clojure.tools.nrepl.server :refer [start-server stop-server]]
   [cider.nrepl                :refer [cider-nrepl-handler]]
   [clojure.tools.logging      :as log]
   [clojure.data.json          :as json]
   [schema.core                :as s]))

;; TODO: add a rest api using http-kit & compojure
(defonce config (atom {:nrepl {:port 4023}}))
(defonce nrepl-server (atom nil))

(defn -main [& args]
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "nrepl is running %s" @config)
  (s/set-fn-validation! true)
  (cnf/load-config!))


