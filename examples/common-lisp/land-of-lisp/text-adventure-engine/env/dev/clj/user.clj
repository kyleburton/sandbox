(ns user
  (:require 
            [mount.core :as mount]
            [text-adventure-engine.figwheel :refer [start-fw stop-fw cljs]]
            text-adventure-engine.core))

(defn start []
  (mount/start-without #'text-adventure-engine.core/repl-server))

(defn stop []
  (mount/stop-except #'text-adventure-engine.core/repl-server))

(defn restart []
  (stop)
  (start))


