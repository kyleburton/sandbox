(ns gherkin.core
  (:require
   [clojure.tools.nrepl.server :refer [start-server stop-server]]
   [cider.nrepl                :refer [cider-nrepl-handler]]
   [clojure.tools.logging      :as log]
   [schema.core                :as s]
   [clojure.core.async         :refer [chan]]))

;; TODO: parse a file into Scenarios
;; TODO: parse a Feature block
;; TODO: parse a Scenario block
;; TODO: parse a Step Line
;; TODO: parse tables
;; TODO: Step Registry

(defonce config
  (atom
   {:nrepl {:port 4019}
    :apps  []}))

(defonce nrepl-server (atom nil))

(defn -main
  "Start the service."
  [& args]
  (s/set-fn-validation! true)
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "nREPL server started: %s" (-> @config :nrepl :port)))

