(ns the-mystery-on-orville-st.web
  (:require
   [clojure.tools.logging :as log]
   [clojure.data.json :as json]
   [org.httpkit.server :as server]))

(defonce config (atom {:port               8027
                       :ip                 "0.0.0.0"
                       :thread             4
                       :worker-name-prefix "orville-web-"
                       :queue-size         20000
                       :max-body           (* 8 1024 1024)
                       :max-line           (* 4 1024)}))

(defonce web-server (atom nil))


(defn app [req]
  (log/infof "app: received request: %s" req)
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body    (json/write-str {:status "OK"
                             :body {:hello "there, how are you?"}})})

(defn start-server! []
  (when @web-server
    (throw (RuntimeException. (format "Error: server is already running! config=%s" @config))))
  (reset! web-server (server/run-server #'app @config)))

(defn stop-server! []
  (when @web-server
    (@web-server :timeout 100)
    (reset! web-server nil)))

(defn init! []
  (start-server!))


(comment
  @config
  @web-server
  (start-server!)
  (stop-server!)

  (do
    (stop-server!)
    (start-server!))

  (log/infof "hey there, does this work?")
  (log/errorf "hey there, does this work?")
  (log/fatalf "hey there, does this work?")

  )
