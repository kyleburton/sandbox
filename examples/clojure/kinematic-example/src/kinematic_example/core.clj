(ns kinematic-example.core
  (:require
   [clojure.tools.nrepl.server :refer [start-server stop-server]]
   [cider.nrepl                :refer [cider-nrepl-handler]]
   [clojure.tools.logging      :as log]
   [schema.core                :as s]
   [kinematic.core             :as kinematic]
   [kinematic.dsl              :as kdsl]
   [org.httpkit.server         :as server]))

(defonce nrepl-server (atom nil))
(defonce config (atom {:nrepl {:port 4011}}))
(defonce stop-server-fn (atom nil))


(kdsl/defweb :example1
  :app-ns-prefix :kinematic-example.core
  :before-middleware []
  :after-middleware []
  :404-page "resources/public/404.html")

(defn -main [& args]
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "nrepl is running %s" @config)
  (s/set-fn-validation! true)
  (reset! stop-server-fn (server/run-server (kdsl/dyn-handler :example1)
                                            {:port 8080})))
