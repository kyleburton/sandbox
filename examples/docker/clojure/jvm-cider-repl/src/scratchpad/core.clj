(ns scratchpad.core
  (:require
   [clojure.tools.nrepl.server                 :refer [start-server stop-server]]
   [cider.nrepl                                :refer [cider-nrepl-handler]]
   [clojure.tools.logging                      :as log]
   [schema.core                                :as s]))

(defonce nrepl-server (atom nil))
(defonce config (atom {:nrepl {:port 4014
                               :bind "0.0.0.0"}}))

(defn -main [& args]
  (reset! nrepl-server (start-server
                        :port (-> @config :nrepl :port)
                        :bind (or (-> @config :nrepl :bind) "127.0.0.1")
                        :handler cider-nrepl-handler))
  (log/infof "nrepl is running %s" @config)
  (s/set-fn-validation! true))

(comment

  (->>
   "/src"
   java.io.File.
   file-seq
   (map str)
   (take 10))


)
