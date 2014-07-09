(ns single-page-app.api.v1.do-something
  [:require
   [clojure.tools.logging    :as log]
   [clojure.data.json :as json]])

(defonce stats (atom {}))

(defn get-handler [request]
  (swap! stats update-in [:requests :get] #(inc (or % 0)))
  (swap! stats update-in [:requests :all] #(inc (or % 0)))
  {:status  200
   :headers {"Content-type" "application/json"}
   :body    (json/json-str {:params (-> request :params)
                            :time   (str (java.util.Date.))
                            :name   (-> request :session :name)
                            :stats  @stats})
   :session (-> request :session)})

(defn post-handler [request]
  (swap! stats update-in [:requests :post] #(inc (or % 0)))
  (let [user-name (or
                   (-> request :params :name)
                   (-> request :session :name))]
    {:status  200
     :headers {"Content-type" "application/json"}
     :body    (json/json-str {:params (-> request :params)
                              :time   (str (java.util.Date.))
                              :name   user-name
                              :stats  @stats})
     :session (->
               request
               :session
               (assoc :name user-name))}))