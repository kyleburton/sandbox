(ns lol-champquiz.server
  (:require
   [lol-champquiz.handler      :refer [app]]
   [config.core                :refer [env]]
   [ring.adapter.jetty         :refer [run-jetty]]
   [clojure.tools.nrepl.server :refer [start-server stop-server]]
   [cider.nrepl                :refer [cider-nrepl-handler]]
   [clojure.tools.logging      :as log]
   [schema.core                :as s])
  (:gen-class))

(def config (atom {:nrepl {:port 4017}
                   :web   {:port "3000"}}))

(def nrepl-server (atom nil))

(defn start-repl! [config]
  (if @nrepl-server
    (log/warnf "Error: repl already running!"))
  (reset! nrepl-server (start-server
                        :port (-> config :port)
                        :handler cider-nrepl-handler)))

(defn -main [& args]
  (start-repl! (-> config deref :nrepl))
  (let [port (Integer/parseInt (or (env :port) (-> config deref :web :port)))]
    (run-jetty app {:port port :join? false})))
