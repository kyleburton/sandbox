(ns spec-sandbox.core
  (:require
   [clojure.spec :as s]
   [clj-time.core :as time]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(s/def ::date inst?)

(s/def ::dateish (s/or
                  ::is-instant    inst?
                  ::is-joda-dtime #(isa? org.joda.time.DateTime (class %1))))

(comment
  (s/valid? ::date (java.util.Date.))
  (s/valid? ::date (clj-time.core/now))

  (s/valid? ::dateish (java.util.Date.))
  (s/valid? ::dateish (clj-time.core/now))
  (s/valid? ::dateish nil)

  (foo "foo")

  (s/conform even? 100)
  (s/conform even? "none")
  (s/conform even? 3.4)
  (s/conform even? nil)

  (mapv (fn [val] [val (s/valid? number? val)]) [100 "none" 3.4 nil])
  [[100 true]
   ["none" false]
   [3.4 true]
   [nil false]]

  (s/valid? number? "this")


  (mapv (fn [val] [val (s/valid? #(and (int? %) (even? %)) val)]) [100 "none" 3.4 nil])
  [[100 true] ["none" false] [3.4 false] [nil false]]  



)
