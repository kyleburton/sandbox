(ns cider-nrepl-scratchpad.core
  (:require
   [clojure.tools.nrepl.server                 :refer [start-server stop-server]]
   [cider.nrepl                                :refer [cider-nrepl-handler]]
   [clojure.tools.logging                      :as log]
   [schema.core                                :as s]
   [clojure.data.json                          :as json]
   [clj-etl-utils.text                         :refer [string->sha256]]
   [clojure.java.io                            :as io]))

(defonce nrepl-server (atom nil))
(defonce config       (atom {:nrepl {:port 4001}}))

(defn -main [& args]
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :handler cider-nrepl-handler))
  (log/infof "nrepl is running as %s" @config)
  (s/set-fn-validation! true))


(def the-time (java.util.Date.))
(defonce was-the-time (java.util.Date.))
(defn some-function [msg]
  (.println System/out (str"Hello: " msg)))

(comment
  (meta (var defonce))
  (some-function "Kevin")

  (do
    (some-function the-time)
    (some-function was-the-time))


  (java.util.Date.)
 
)
