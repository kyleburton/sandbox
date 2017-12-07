(ns text-adventure-engine.env
  (:require [selmer.parser :as parser]
            [clojure.tools.logging :as log]
            [text-adventure-engine.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[text-adventure-engine started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[text-adventure-engine has shut down successfully]=-"))
   :middleware wrap-dev})
