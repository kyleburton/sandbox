(ns the-mystery-on-orville-st.core
  (:require
   [nrepl.server          :refer [start-server start-server]]
   [cider.nrepl           :refer [cider-nrepl-handler]]
   [clojure.tools.logging :as log]
   [schema.core           :as s]
   [clojure.data.json     :as json]))

(defonce nrepl-server (atom nil))
(defonce config (atom {:nrepl {:port 4027}}))

(defn -main [& args]
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "nrepl is running %s" @config)
  (s/set-fn-validation! true)
  (spit ".config.json"
        (-> config deref json/write-str)))
