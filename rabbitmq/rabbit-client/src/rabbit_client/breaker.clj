(ns rabbit-client.breaker
  (:import
   [com.github.kyleburton.teporingo BreakerOpenException])
  (:require
   [clj-etl-utils.log :as log])

  (:use
   [clj-etl-utils.lang-utils :only [raise aprog1]]))

(defn basic-breaker
  ([inner-fn]
     (basic-breaker inner-fn {:retry-after 2}))
  ([inner-fn opts]
     (let [state (atom (merge {:closed? true
                               :retry-after 2
                               :current-retries 0}
                              opts))]
       (fn [& args]
         (cond
           (:closed? @state)
           (try
            (apply inner-fn args)
            (catch Exception ex
              (log/errorf ex (str ex))
              (swap! state
                     assoc
                     :closed? false
                     :current-retries 0)
              (throw (BreakerOpenException.
                      (format "Breaker[%s] Opened by: %s" @state ex)
                      ex))))
           (>= (:current-retries @state)
               (:retry-after     @state))
           (try
            (aprog1
                (apply inner-fn args)
              (swap!
               state
               assoc
               :closed? true
               :current-retries 0))
            (catch Exception ex
              (log/errorf ex (str ex))
              (swap! state
                     assoc
                     :closed? false
                     :current-retries 0)
              (throw (BreakerOpenException.
                      (format "Breaker[%s] Opened during re-attempt by: %s" @state ex)
                      ex))))
           :else
           (do
             (swap!
              state
              update-in
              [:current-retries]
              inc)
             (throw (BreakerOpenException. (format "Breaker[%s] (still) Open." @state)))))))))

(defmacro defbreaker [type fname argspec & body]
  (let [sym (symbol (str (name type) "-breaker"))]
    `(def ~fname
          (~sym
           (fn ~argspec
             ~@body)))))

(comment

 (def stuff (basic-breaker
             (fn [r]
               (if (= 1 (.nextInt (java.util.Random.) 3))
                 (raise "sorry, you loose"))
               r)))

 (defbreaker :basic stuff2 [r]
   (if (= 1 (.nextInt (java.util.Random.) 3))
     (raise "sorry, you loose"))
   r)

 (stuff2 :a)

 )






