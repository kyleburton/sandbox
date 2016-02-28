(ns cider-nrepl-scratchpad.web
  (:require
   [org.httpkit.server    :as server]
   [schema.core           :as s]
   [clojure.tools.logging :as log]
   [clj-time.core         :as time]))


(s/defn async-handler [ring-request]
  ;; unified API for WebSocket and HTTP long polling/streaming
  (server/with-channel ring-request channel    ; get the channel
    (if (server/websocket? channel)            ; if you want to distinguish them
      (do
        (log/infof "request: is a websocket, deferring to server/on-receive...")
        (server/on-receive channel (fn [data] ; two way communication
                                     (server/send! channel data))))
      (do
        (log/infof "request: standard http, defering to ring-handler")
        (server/send! channel {:status 200
                               :headers {"Content-Type" "text/plain"}
                               :body    "Long polling?"})))))

(defonce web-server (atom nil))

(def Config {:port s/Num})

(s/defn stop!
  ([]
   (stop! web-server))
  ([web-server]
   (when (and @web-server (-> @web-server :stop-fn))
     (log/infof "stopping httpkit web server: %s" @web-server)
     ((-> @web-server :stop-fn) :timeout 100)
     (reset! web-server nil))))

(s/defn restart!
  ([]
   (restart! web-server {:port 8080}))
  ([web-server :- clojure.lang.Atom config :- Config]
   (stop! web-server)
   (reset! web-server
           {:config     config
            :started-at (time/now)
            :stop-fn    (server/run-server async-handler config)}))) ; Ring server


(comment
  (restart!)

  )
