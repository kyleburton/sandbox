(ns amidst-minecraft.core
  (:require
   [nrepl.server          :refer [start-server start-server]]
   [cider.nrepl           :refer [cider-nrepl-handler]]
   [clojure.tools.logging :as log]
   [clojure.pprint        :as pprint]))

(defonce nrepl-server (atom nil))
(defonce config (atom {:nrepl {:port 4046}}))


(defn -main [& args]
  (.setLevel (org.slf4j.LoggerFactory/getLogger org.slf4j.Logger/ROOT_LOGGER_NAME) ch.qos.logback.classic.Level/INFO)
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "amidst-minecraft.core/main: nrepl started on port=%s" (-> @config :nrepl :port))
  (log/infof "amidst-minecraft.core/main: args=%s" args))
