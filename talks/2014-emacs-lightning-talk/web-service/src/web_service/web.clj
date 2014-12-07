(ns web-service.web
  (:require
   [clojure.tools.logging          :as log]
   [ring.middleware.session        :as ring-session]
   [ring.middleware.session.cookie :refer [cookie-store]]
   [ring.middleware.params         :refer [wrap-params]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [org.httpkit.server             :as httpkit-server]
   [kinematic.http.middleware      :refer [wrap-kinematic]]
   [ring.adapter.jetty             :as jetty]
   [web-service.session            :refer [wrap-with-session-binding]]))

(defonce apps (atom {}))

(defn main-handler [request]
  {:status  404
   :headers {"Content-type" "text/plain"}
   :body    "There is nothing here yet."})

(comment
  ;; NB: this is how I generated the cookie key
  (mapv
   (fn [_] (int (rand 127)))
   (range 16))
  
  )

(def cookie-secret-key
  (let [vals  [64 113 84 28 76 53 25 77 27 102 38 12 23 49 105 67]
        bytes (byte-array 16)]
    (dotimes [ii 16]
      (aset
       bytes ii
       (byte (get vals ii))))
    bytes))

(defn make-handler [app]
  (-> main-handler
      (wrap-kinematic app)
      wrap-with-session-binding
      (ring-session/wrap-session
       {:cookie-name "sa-session"
        :store       (cookie-store
                      {:key cookie-secret-key})})
      wrap-keyword-params
      wrap-params))

(defn init [config]
  (reset! apps {})
  (doseq [app (-> config :apps)]
    (swap! apps assoc
           (:app app)
           {:config app
            :server 
            (jetty/run-jetty
             (make-handler app)
             {:port (:port app)
              :join? false})
            ;; Is it http kit's server that's leaking file descriptors?
            #_(httpkit-server/run-server
               (make-handler app)
               {:port (:port app)})})
    (log/infof "started: %s" (:app app))))

(defn stop-all [config]
  (doseq [[name app] @apps]
    ((-> app :server) :timeout 100)))

(defn restart [config]
  (stop-all config)
  (init @config))


