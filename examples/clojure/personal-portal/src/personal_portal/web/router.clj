(ns personal-portal.web.router
  (:require
   [clojure.tools.logging :as log]
   [clojure.data.json     :as json]))

;; NB: the implication of this approach (regsitration at compile time)
;; is that we can't AOT our route handlers

;; TODO: consider implementing a suffix map based caching of the function & name resolution
;; TODO: support middleware per namespace or route

(comment

  ;; simplest example, set of uri matchers, supports parameterization
  (router/register!
   {:uris ["/" "/:id"]})

  ;; different middleware for each uri
  (router/register!
   {:uris [{:uri        "/"
            :middleware [wrap-thing1]}
           {:uri        "/:id"
            :middleware [wrap-thing2]}]})

  ;; middleware around all uris
  (router/register!
   {:uris       ["/" "/:id"]
    :middleware []})

  )

(defonce routes (atom {:uris    []
                       :routes  {}}))

(defn parse-route-info [route-ns ^String uri]
  (let [ns-parts  (as-> route-ns it (str it) (.split it "\\.") (vec it)
                        (drop 2 it)
                        (vec it))]
    (as-> ns-parts it
      (concat it (.split uri "/"))
      (filter #(not= % "") it)
      (mapv (fn [^String elt]
              (cond
                (.startsWith elt ":")
                (keyword (.substring elt 1))
                :otherwise
                elt))
            it))))

(defn register* [opts]
  (doseq [uri (or (::uris opts)
                  (:uris opts))]
    (log/infof "registering: opts=%s" opts)
    ;; TDOO: check for dupes and throw when we find them (prevents being surprised by being overriden)
    (swap! routes
           (fn [routes uri opts]
             (let [parts (parse-route-info (:ns opts) uri)]
               {:uris    (conj  (:uris routes)    [parts opts])
                :routes  (assoc (:routes routes)  (:ns opts) opts)}))
           uri opts))
  :ok)

(defmacro register! [& [opts]]
  `(~register* (assoc ~(or opts {}) :ns *ns*)))

(defmacro defapi [http-method uris args & body]
  (let [handler-name (-> http-method name #_(format "%-handler") symbol)]
    `(do
       (register! {::uris ~uris})
       (defn ~handler-name ~args
         ~@body))))

(defmacro defget [fname uris args & body]
  `(do
     (register! {::uris ~uris})
     (defn ~fname ~args
       ~@body)))

(defmacro defpost [fname uris args & body]
  `(do
     (register! {::uris ~uris})
     (defn ~fname ~args
       ~@body)))

(comment
  (-> ::get name symbol)

  )


(defmulti parse-uri (fn [uri-info]
                      (cond
                        (isa? (class uri-info) String)
                        ::string

                        ;; TODO: do we want to support this & what would it look like?
                        ;; (seqable? uri-info)
                        ;; ::seq

                        (map? uri-info)
                        ::map

                        (nil? uri-info)
                        ::nil

                        :otherwise
                        ::unknown)))

(defmethod parse-uri ::nil [uri-info]
  nil)

(defmethod parse-uri ::map [uri-info]
  (parse-uri (:uri uri-info)))

(defmethod parse-uri ::string [uri-info]
  (as-> uri-info it
    (.split ^String it "/")
    (filter #(not= % "") it)
    (vec it)))


(defmethod parse-uri ::unknown [uri-info]
  (throw (RuntimeException. (format "parse-uri: error, unrecognized uri-info type: class=%s; uri-info=%s"
                                    (class uri-info)
                                    uri-info))))


(defn route-matches? [route-parts uri-parts]
  (loop [match-info                 {}
         [route-part & route-parts] route-parts
         [uri-part & uri-parts]     uri-parts]
    (log/infof "route-matches: route/uri %s=%s; route-parts=%s; uri-parts=%s"
               route-part uri-part route-parts uri-parts)
    (cond
      (and (not route-part)
           (not uri-part))
      match-info

      ;; TODO: we need an assertion somewhere that the :* is the last of the parts ...
      (= :* route-part)
      (assoc match-info :* (vec (cons uri-part uri-parts)))

      (and
       (keyword? route-part)
       uri-part)
      (recur (assoc match-info route-part uri-part)
             route-parts
             uri-parts)

      (= route-part uri-part)
      (recur match-info route-parts uri-parts)

      :otherwise
      nil)))

(comment
  ;; find-handler: uri=/api/v1/info; route-info=[["api" "v1" "info" :id
  (route-matches? ["api" "v1" "info" :id] (parse-uri "/api/v1/info"))
  (route-matches? ["api" "v1" "info" :id] (parse-uri "/api/v1/info"))

  (route-matches? ["api" "v1" "info"] (parse-uri "/api/v1/info"))
  (route-matches? ["api" "v1" "info"] (parse-uri "/api/v1/info/"))

  (route-matches? ["api" "v1" "info" :id] (parse-uri "/api/v1/info/"))
  (route-matches? ["api" "v1" "info" :id] (parse-uri "/api/v1/info/asdf"))

  (route-matches? ["api" "v1" "info"] (parse-uri "/api/v1/info"))
  (route-matches? ["api" "v1" "person" :id "first-name"] (parse-uri "/api/v1/info"))
  (route-matches? ["api" "v1" "person" :id "first-name"] (parse-uri "/api/v1/person/1234/first-name"))
  (route-matches? ["api" "v1" "info" :*] (parse-uri "/api/v1/info/more/stuff/here"))

  )

(defn find-handler [request router-uris]
  (let [parsed-uri (-> request :uri parse-uri)]
    (->>
     router-uris
     (map (fn [route-info]
            (let [match-info
                  [(route-matches? (first route-info) (-> request :uri parse-uri))
                   (second route-info)]]
              (log/infof "find-handler: uri=%s; route-info=%s; match-info=%s"
                         (-> request :uri) route-info match-info)
              match-info)))
     (filter first)
     first)))

(comment
  (find-handler request (-> routes deref :uris))

  )

(defn keyword->symbol [k]
  (-> k name symbol))

(defn invoke-handler [handler-fn request]
  (let [res (handler-fn request)]
    (cond
      (and (map? res)
           (-> res :body map?))
      (assoc res :body (json/write-str (:body res)))

      :otherwise
      res)))

(defn invoke-route [request match-info]
  (let [handler-fn (ns-resolve (-> match-info second :ns) (-> request :request-method keyword->symbol))]
    (cond
      handler-fn
      (invoke-handler handler-fn (assoc request :route-params (first match-info)))

      :otherwise
      (do
        (log/warnf "invoke-route: NO HANDLER FUNCTION request=%s; match-info=%s" request match-info)
        {:status  404
         :headers {"Content-type" "application/json"}
         :body    "{\"status\": \"NotFound\"}"}))))

(defn handler [request]
  (let [match-info (find-handler request (-> routes deref :uris))]
    (cond
      match-info
      (invoke-route request match-info)

      :otherwise
      (do
        (log/info "handler: no matching route found: request=%s" request)
        {:status  404
         :headers {"Content-type" "application/json"}
         :body    "{\"status\": \"NotFound\"}"}))))


(comment
  (personal-portal.web/restart!)

  (ns-resolve 'personal-portal.web.api.v1.info 'get)
  request

  @routes
  )
