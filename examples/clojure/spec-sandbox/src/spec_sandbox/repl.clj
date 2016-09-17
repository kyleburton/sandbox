(ns spec-sandbox.repl
  (:require
   [clojure.tools.nrepl.server                 :refer [start-server stop-server]]
   [cider.nrepl                                :refer [cider-nrepl-handler]]
   [clojure.tools.logging                      :as log]))


(defonce nrepl-server (atom nil))
(defonce config (atom {:nrepl {:port 4019}}))

(defn -main [& args]
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "nrepl is running %s" @config))
