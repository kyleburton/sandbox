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


(s/def ::dateish?
  (s/nilable
   (s/or
    ::is-instant    inst?
    ::is-joda-dtime #(isa? org.joda.time.DateTime (class %1)))))

(comment
  (s/valid? ::date (java.util.Date.))
  (s/valid? ::date (clj-time.core/now))

  (s/valid? ::dateish (java.util.Date.))
  (s/valid? ::dateish (clj-time.core/now))
  (s/valid? ::dateish nil)

  
  (mapv (fn [val] [val (s/valid? ::dateish? val)])
        [::keyword "string" (clj-time.core/now) nil
         (java.util.Date.)])
  ;; => 
  ;; [[:spec-sandbox.core/keyword false]
  ;;  ["string" false]
  ;;  [#object[org.joda.time.DateTime 0x43069f6d "2016-09-17T21:18:18.518Z"] true]
  ;;  [nil true]
  ;;  [#inst "2016-09-17T21:18:18.518-00:00" true]]
  
  (s/explain-str ::dateish? :stuff)
  "val: :stuff fails spec: :spec-sandbox.core/dateish? at: [:clojure.spec/nil] predicate: nil?\nval: :stuff fails spec: :spec-sandbox.core/dateish? at: [:clojure.spec/pred :spec-sandbox.core/is-instant] predicate: inst?\nval: :stuff fails spec: :spec-sandbox.core/dateish? at: [:clojure.spec/pred :spec-sandbox.core/is-joda-dtime] predicate: (isa? org.joda.time.DateTime (class %))\n"


  (s/explain-data ::dateish? :stuff)
  #:clojure.spec{:problems ({:path [:clojure.spec/nil], :pred nil?, :val :stuff, :via [:spec-sandbox.core/dateish?], :in []} {:path [:clojure.spec/pred :spec-sandbox.core/is-instant], :pred inst?, :val :stuff, :via [:spec-sandbox.core/dateish?], :in []} {:path [:clojure.spec/pred :spec-sandbox.core/is-joda-dtime], :pred (isa? org.joda.time.DateTime (class %)), :val :stuff, :via [:spec-sandbox.core/dateish?], :in []})}

  {:this {:that "other" :thing "stuff"}}

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
