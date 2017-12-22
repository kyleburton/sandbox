(ns the-mystery-on-orville-st.web
  (:require
   [clojure.tools.logging        :as log]
   [clojure.data.json            :as json]
   [org.httpkit.server           :as server
                                 :refer [send! with-channel on-close on-receive]]
   [compojure.core               :as compojure
                                 :refer [GET defroutes wrap-routes]]
   [ring.middleware.anti-forgery :refer [wrap-anti-forgery]]
   [selmer.parser                :as parser]
   [selmer.filters               :as filters]
   [markdown.core                :refer [md-to-html-string]]
   [ring.util.http-response      :refer [content-type ok]]
   [ring.util.anti-forgery       :refer [anti-forgery-field]]
   [ring.middleware.anti-forgery :refer [*anti-forgery-token*]]
   [cprop.core                   :refer [load-config]]
   [cprop.source                 :as source]
   [mount.core                   :refer [args defstate]])
  (:import [javax.servlet ServletContext]
           [org.joda.time ReadableInstant]))

;; https://yogthos.net/posts/2015-06-11-Websockets.html

(defonce config (atom {:port               8027
                       :ip                 "0.0.0.0"
                       :thread             4
                       :worker-name-prefix "orville-web-"
                       :queue-size         20000
                       :max-body           (* 8 1024 1024)
                       :max-line           (* 4 1024)}))

(defonce web-server (atom nil))

(defonce channels (atom #{}))

(defn connect! [channel]
  (log/infof "connection established: %s" channel)
  (swap! channels conj channel))

(defn disconnect! [channel status]
  (log/infof "client disconnected, channel=%s status=%s" channel status)
  (swap! channels #(remove #{channel} %)))

(defn notify-clients [msg]
  (log/infof "posting msg=%s to all clients" msg)
  (doseq [channel @channels]
    (send! channel msg)))

(defn ws-handler [req]
  (with-channel req channel
    (connect! channel)
    (on-close channel (partial disconnect! channel))
    (on-receive channel #(notify-clients %))))

(defn index-handler [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body    (json/write-str {:status "OK"
                             :body {:hello "there, how are you?"}})})

(defroutes ws-routes
  (GET "/ws" request (ws-handler request)))

(defroutes routes
  ;; NB: index should send the html + precompiled JS
  (GET "/" request (index-handler request))
  ;; TODO: add a route for static assets
  ;; TODO: the remaining routes should all be json over http apis
  )

(declare ^:dynamic *app-context*)
(parser/set-resource-path!     (clojure.java.io/resource "templates"))
(parser/add-tag! :csrf-field   (fn [_ _] (anti-forgery-field)))
(filters/add-filter! :markdown (fn [content] [:safe (md-to-html-string content)]))

(defn render
  "renders the HTML template located relative to resources/templates"
  [template & [params]]
  (content-type
   (ok
    (parser/render-file
     template
     (assoc params
            :page template
            :csrf-token *anti-forgery-token*
            :servlet-context *app-context*)))
   "text/html; charset=utf-8"))

(defn error-page
  "error-details should be a map containing the following keys:
   :status - error status
   :title - error title (optional)
   :message - detailed error message (optional)

   returns a response map with the error page as the body
   and the status specified by the status key"
  [error-details]
  {:status  (:status error-details)
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body    (parser/render-file "error.html" error-details)})


(defstate env :start (load-config
                      :merge
                      [(args)
                       (source/from-system-props)
                       (source/from-env)]))

(defn wrap-context [handler]
  (fn [request]
    (binding [*app-context*
              (if-let [context (:servlet-context request)]
                ;; If we're not inside a servlet environment
                ;; (for example when using mock requests), then
                ;; .getContextPath might not exist
                (try (.getContextPath ^ServletContext context)
                     (catch IllegalArgumentException _ context))
                ;; if the context is not specified in the request
                ;; we check if one has been specified in the environment
                ;; instead
                (:app-context env))]
      (handler request))))

(defn wrap-internal-error [handler]
  (fn [req]
    (try
      (handler req)
      (catch Throwable t
        (log/error t)
        (error-page {:status 500
                     :title "Something very bad has happened!"
                     :message "We've dispatched a team of highly trained gnomes to take care of the problem."})))))

(defn wrap-csrf [handler]
  (wrap-anti-forgery
   handler
   {:error-response
    (error-page
     {:status 403
      :title "Invalid anti-forgery token"})}))

(defn wrap-base [handler]
  (-> ((:middleware defaults) handler)
      wrap-webjars
      (wrap-defaults
       (-> site-defaults
           (assoc-in [:security :anti-forgery] false)
           (assoc-in  [:session :store] (ttl-memory-store (* 60 30)))))
      wrap-context
      wrap-internal-error))

(def app
  (-> (routes
       websocket-routes
       (wrap-routes home-routes wrap-csrf)
       base-routes)
      wrap-base))

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
