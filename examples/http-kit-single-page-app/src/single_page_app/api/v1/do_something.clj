(ns single-page-app.api.v1.do-something
  [:require
   [clojure.tools.logging    :as log]
   [clojure.data.json :as json]])

(defonce stats (atom {}))

(defn response-body [m]
  (let [resp-time (java.util.Date.)]
    (->
     m
     (assoc
         :stats     @stats
         :time      (str resp-time)
         :time_ms   (.getTime resp-time)
         :time_nano (System/nanoTime))
     json/json-str)))

(defn get-handler [request]
  (def request request)
  (swap! stats update-in [:requests :get] #(inc (or % 0)))
  (swap! stats update-in [:requests :all] #(inc (or % 0)))
  (let [num1 (-> request :query-params (get "num1") Integer/parseInt)
        num2 (-> request :query-params (get "num2") Integer/parseInt)]
   {:status  200
    :headers {"Content-type" "application/json"}
    :body    (response-body {:params (-> request :params)
                             :name   (-> request :session :name)
                             :answer (+ num1 num2)})
    :session (-> request :session)}))

(defn post-handler [request]
  (swap! stats update-in [:requests :post] #(inc (or % 0)))
  (let [body (slurp (:body request))
        _ (def body body)
        user-name (or
                   (-> request :params :name)
                   (-> request :session :name))]
    {:status  200
     :headers {"Content-type" "application/json"}
     :body    (response-body {:params (-> request :params)
                              :name   (-> request :session :name)})
     :session (->
               request
               :session
               (assoc :name user-name))}))