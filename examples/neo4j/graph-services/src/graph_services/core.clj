(ns graph-services.core
  (:require
   [clojure.tools.nrepl.server :refer [start-server stop-server]]
   [cider.nrepl                :refer [cider-nrepl-handler]]
   [clojure.tools.logging      :as log]
   [schema.core                :as s]))

(defonce nrepl-server (atom nil))

(defonce config
  (atom
   {:nrepl {:port 4474}
    :apps  []
    :neo4j {:host "localhost"
            :port 7474}}))

(defn neo4j-url []
  (format "http://%s:%s/db/data"
          (-> config deref :neo4j :host)
          (-> config deref :neo4j :port)))

(defn init [config]
  :ok)

(defn restart [config]
  (init config))

(defn -main
  "Start the service."
  [& args]
  (init config)
  (s/set-fn-validation! true)
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "nREPL server started: %s" (-> @config :nrepl :port)))



(comment
  (restart config)

  (-main [])

  )
