(ns personal-portal.core
  (:require
   [nrepl.server          :refer [start-server start-server]]
   [cider.nrepl           :refer [cider-nrepl-handler]]
   [clojure.tools.logging :as log]
   [clojure.spec.alpha    :as s]
   [personal-portal.web   :as web]
   personal-portal.web.api.v1.info))

(defonce nrepl-server (atom nil))
(defonce config (atom {:nrepl {:port 4000}}))

(defn -main [& args]
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "nrepl is running %s" @config)
  #_(web/restart!))
