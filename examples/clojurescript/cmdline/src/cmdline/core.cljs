(ns cmdline.core
  (:require
   [cljs.nodejs    :as nodejs]
   [cmdline.config :as config]))

(nodejs/enable-util-print!)

(def http (js/require "http"))

(def server (atom nil))

(defn -main [config-file]
  (when config-file
    (println (str "Config File: " config-file))
    (config/load-config! config-file))
  (println (str "Config: " (-> config/config deref clj->js js/JSON.stringify)))
  (swap!
   server
   (.createServer
    http
    (fn [request response]
      ;; res.writeHead (200, {'Content-Type': 'text/plain'});
      (.writeHead response 200 #js {"Content-Type" "text/plain"})
      (.end response "Hello node web server!")
      )))
  (.listen @server (-> config/config deref :port)))

(set! *main-cli-fn* -main)
