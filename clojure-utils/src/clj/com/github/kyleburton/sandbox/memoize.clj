;; my personal memoization extensions
(ns com.github.kyleburton.sandbox.memoize
  (:require [com.github.kyleburton.sandbox.utils :as kutils]))

(defn make-once-only [f]
  "Memoize a function so it is only invoked once, regardless of arguments."
  (let [atm (atom nil)]
    (fn [& args]
      (if (not @atm)
        (do
          (prn "resetting the atom, args=" args)
          (reset! atm (apply f args))
          @atm))
      @atm)))

(defmacro def-once-only [name & args]
  `(def ~name (make-once-only (fn ~@args))))

(def *store-registry* (atom { }))

(defn register-store [name fn]
  (reset! *store-registry* (assoc @*store-registry* name fn)))

(def memoize-map-store 
     (let [cache (atom {})]
       (fn [op key & [val]] 
         (cond (= :get op)
               (@cache key)
               (= :chk op)
               (contains? @cache key)
               (= :put op)
               (do
                 (reset! cache (assoc @cache key val))
                 (@cache key))
               true
               (throw (RuntimeException. (format "memoize-map-store: error, unknown op: %s" op)))))))

(register-store :default memoize-map-store)

;; (memoize-map-store :get :a)
;; (memoize-map-store :chk :a)
;; (memoize-map-store :put :a 1)

;;
;; :store is a function that implements the store/retreive of
;;  unserialized data - persistence is a concern of the store
;;
;; :key is a function for generating the store key
;; 

;; TODO: HERE: need to complete this
;; (defn memoize-with-store [fn & params]
;;   (let [args    (kutils/pairs->map params)
;;         keyfn   (:key args kutils/freeze)
;;         store   (if (keyword? (:store args))
;;                   (*store-registry* (:store args)
;;                                     (:default *store-registry*)))]
;;     (fn [& args]
;;       (let [k (keyfn args)]
;;         (cond (store :chk k)
;;               (store :get k)
;;               true
;;               (do
;;                 (store :put k (apply fn args))
;;                 (store :get k)))))




;; (doall
;;  (doseq [provider (seq (java.security.Security/getProviders))
;;          service (seq (.getServices provider))]
;;    (prn (format "provider:%s service:%s" provider service))))

;; (let [provider (first (first (map #(vec (.toArray (.getServices %))) (vec (java.security.Security/getProviders)))))]
;;   (prn (format "algorithm: %s" (.getAlgorithm provider)))
;;   (prn (format "type: %s" (.getType provider)))
;;   (prn (format "provider: %s" (.getName (.getProvider provider))))
;;   (prn (format "class-name: %s" (.getClassName provider))))

;; (defn make-memoize-dir-store [path]
;;   (fn [op key & [val]] 
;;     (.mkdirs (java.io.File. path))
;;     (let [fname (java.io.File. (str path "/" key))]
;;       (cond (= :get op)
;;             (prn "pull from disk: " fname)
;;             (= :chk op)
;;             (prn "exists? on disk: " fname)
;;             (= :put op)
;;             (prn "store to disk: " fname)
;;             true
;;             (throw (RuntimeException. (format "memoize-map-store: error, unknown op: %s" op)))))))

;; (def mmds (make-memoize-dir-store "/home/mortis/tmp/mem.clj"))
;; (mmds :put "foo" 1)