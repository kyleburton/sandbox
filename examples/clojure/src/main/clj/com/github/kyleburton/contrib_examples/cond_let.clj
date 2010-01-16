(ns com.github.kyleburton.contrib-examples.cond-let
  (:use [clojure.contrib.cond])
  (:gen-class))


(defn main [args]
  (doseq [arg args]
    (cond-let [result]
      (= arg "a")
      (printf "first-clause:  arg=%s result=%s\n" arg result)
      (= arg "b")
      (printf "second-clause: arg=%s result=%s\n" arg result)
      :else
      (printf "else-clause:   arg=%s\n" arg))))


(comment

  (com.github.kyleburton.contrib-examples.cond-let/main (into-array String ["this" "a" "c"]))

  (clojure.lang.Compile/main (into-array String ["cond-let.clj"]))

  (let [the-val :a]
    (cond-let [result]
      (= the-val :a)
      [:first result]
      (= the-val :b)
      [:second result]
      :else
      [:else]))

  )

