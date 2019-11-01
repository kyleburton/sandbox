(ns personal-portal.web
  (:require
   [org.httpkit.server         :as server]
   [personal-portal.web.router :as router]
   [compojure.core             :refer [defroutes GET PUT POST DELETE ANY context]]
   [clojure.tools.logging      :as log]))

(defonce server (atom {}))
(def default-props {:server {:port 8080}})

;; TODO: if body is a map or a sequence, automatically convert it to a string w/json/write-str

(defn start!
  ([]
   (start! {}))
  ([props]
   (let [props (merge default-props {})]
     (reset! server {:props props
                     :server (server/run-server router/handler (:server props))}))
   (log/infof "web server started: %s" @server)
   @server))

(defn stop! []
  (let [props   (-> server deref :props)
        stop-fn (-> server deref :server)]
    (and stop-fn (stop-fn))
    (swap! server dissoc :server)
    props))

(defn restart! []
  (stop!)
  (start! (-> server :props)))


(comment
  (restart!)


  @server

  )
