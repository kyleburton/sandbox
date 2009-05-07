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

