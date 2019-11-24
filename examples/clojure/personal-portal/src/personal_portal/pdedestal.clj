(ns personal-portal.pdedestal
  (:require
   [io.pedestal.http                     :as http]
   [io.pedestal.http.route               :as route]
   [io.pedestal.test                     :as test]
   [io.pedestal.http.content-negotiation :as conneg]
   [clojure.data.json                    :as json]))


(defn response [status body & {:as headers}]
  {:status status :body body :headers headers})

(def ok       (partial response 200))
(def created  (partial response 201))
(def accepted (partial response 202))

(def echo
  {:name :echo
   :enter
   (fn [context]
     (let [request (:request context)
           response (ok context)]
       (assoc context :response response)))})


(defn hello-view-impl [context]
  (def context context)
  (let [response (ok context)]
    (assoc response :body {:message "Hi there, this is the hello-view"})))

(defn hello-view [context]
  (#'hello-view-impl context))

(defn parse-shim-body [^String body]
  (as-> body it
    (.split it "\\n")
    (map #(vec (.split ^String  % ":" 2)) it)
    (reduce
     (fn [acc [k v]]
       (def acc acc)
       (def k k)
       (def v v)
       (cond
         (= k "ARG")
         (update-in acc [:args] conj v)

         (= k "STDIN")
         (assoc acc :stdin v)

         (= k "STDOUT")
         (assoc acc :stdout v)

         (= k "STDERR")
         (assoc acc :stderr v)

         (= k "PWD")
         (assoc acc :pwd v)

         :otherwise
         (update-in acc [:other] conj v)))
     {:args []
      :stdin nil
      :stdout nil
      :stderr nil
      :pwd nil
      :other []}
     it)))

(defn clishim-impl [context]
  (def context context)
  (let [response (ok context)
        body     (-> context :body slurp)
        parsed   (parse-shim-body body)]
    (def body body)
    ;; (count body)
    (-> parsed :stdout (spit "direct, to your stdout\n") )
    (-> parsed :stderr (spit "direct, to your stderr\n") )
    (assoc response :body {:message "Ok ok ok, lets do something"})))

(defn clishim [context] (#'clishim-impl context))

(def supported-types ["text/html" "application/edn" "application/json" "text/plain"])

(def content-neg-intc (conneg/negotiate-content supported-types))

(defn accepted-type
  [context]
  (get-in context [:request :accept :field] "text/plain"))

(defn transform-content [body content-type]
  (case content-type
    "text/html"        body
    "text/plain"       body
    "application/edn"  (pr-str body)
    "application/json" (json/write-str body)))

(defn coerce-to
  [response content-type]
  (-> response
      (update :body transform-content content-type)
      (assoc-in [:headers "Content-Type"] content-type)))

(def coerce-body
  {:name ::coerce-body
   :leave
   (fn [context]
     (if (get-in context [:response :headers "Content-Type"])
       context
       (update-in context [:response] coerce-to (accepted-type context))))})


(def routes
  (route/expand-routes
   #{["/hello"                   :get    [coerce-body content-neg-intc hello-view]    :route-name :hello-view]
     ["/clishim"                 :post   [coerce-body content-neg-intc clishim]       :route-name :clishim]

     ["/todo"                    :post   [coerce-body content-neg-intc echo]  :route-name :list-create]
     ["/todo"                    :get    [coerce-body content-neg-intc echo]  :route-name :list-query-form]
     ["/todo/:list-id"           :get    [coerce-body content-neg-intc echo]  :route-name :list-view]
     ["/todo/:list-id"           :post   [coerce-body content-neg-intc echo]  :route-name :list-item-create]
     ["/todo/:list-id/:item-id"  :get    [coerce-body content-neg-intc echo]  :route-name :list-item-view]
     ["/todo/:list-id/:item-id"  :put    [coerce-body content-neg-intc echo]  :route-name :list-item-update]
     ["/todo/:list-id/:item-id"  :delete [coerce-body content-neg-intc echo]  :route-name :list-item-delete]}))

(def service-map
  {::http/routes routes
   ::http/type   :jetty
   ::http/port   8890})

(defn start []
  (http/start (http/create-server service-map)))

(defonce server (atom nil))

(defn start-dev []
  (reset! server
          (http/start (http/create-server
                       (assoc service-map
                              ::http/join? false)))))

(defn stop-dev []
  (and @server (http/stop @server)))

(defn restart []
  (stop-dev)
  (start-dev))


(comment
  (restart)

  )
