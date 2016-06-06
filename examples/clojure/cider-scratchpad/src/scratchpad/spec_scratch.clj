(ns scratchpad.spec-scratch
  (:require
   [clojure.spec :as s]))


(comment

  ;; any single-arg -> boolean function can be a predicate
  (s/conform even? 100)
  100
  (s/conform even? 101)
  :clojure.spec/invalid

  (s/valid? even? 101)
  false


  (s/conform (complement nil?) nil)
  :clojure.spec/invalid
  (s/conform (complement nil?) "yes")
  "yes"


  (import java.util.Date)
  (s/valid? #(instance? Date %) (Date.))
  true

  (s/valid? #{:club :diamond :heart :spade} :club)
  true


  ;; registry

  (s/def ::date #(instance? Date %))
  (s/def ::suit #{:club :diamond :heart :spade})

  (s/valid? ::date (Date.))
  true
  (s/valid? ::date "2016-05-26")
  false

  (s/def ::big-even (s/and integer? even? #(> % 1000)))

  (s/valid? ::big-even 10000)


  (s/def ::name-or-id (s/or :id integer?
                               :name string?))

  (s/valid? ::name-or-id 22)
  true
  (map (fn [v] [v (s/valid? ::name-or-id v)]) [1 "three" 44 2.33 :forty-two])
  ([1 true] ["three" true] [44 true] [2.33 false] [:forty-two false])

  (map (fn [v] (s/conform ::name-or-id v)) [1 "three" 44 2.33 :forty-two])
  ([:id 1]
   [:name "three"]
   [:id 44]
   :clojure.spec/invalid
   :clojure.spec/invalid)


  (s/valid? (s/nilable string?) nil)
  true

  (s/explain-data ::name-or-id :foo)
  {:clojure.spec/problems {[:id]   {:pred integer?, :val :foo, :via [], :in []},
                           [:name] {:pred string?, :val :foo, :via [], :in []}}}

  (s/def ::ingredient (s/cat :quantity number? :unit keyword?))
  (s/conform ::ingredient [2 :teaspoon])

  (s/explain-data ::ingredient [11 "peaches"])
  {:clojure.spec/problems {[:unit] {:pred keyword?, :val "peaches", :via [], :in [1]}}}

  (s/explain-data ::ingredient [11])
  {:clojure.spec/problems {[:unit] {:reason "Insufficient input", :pred keyword?, :val (), :via [], :in []}}}


  (s/def ::seq-of-keywords (s/* keyword?))
  (s/conform ::seq-of-keywords [:a :b :c])

  (s/def ::odds-then-maybe-even (s/cat
                                 :odds (s/+ odd?)
                                 :even (s/? even?)))
  (s/conform ::odds-then-maybe-even [1 3 5 7 22])
  {:odds [1 3 5 7], :even 22}
  (s/valid? ::odds-then-maybe-even [1 3 5 7 22])
  true

  (s/explain-data ::odds-then-maybe-even [4])
  {:clojure.spec/problems {[:odds] {:pred odd?, :val 4, :via [:scratchpad.spec-scratch/odds-then-maybe-even], :in [0]}}}

  ;; opts are alternating keywords and booleans
  (defn boolean? [b] (instance? Boolean b))
  (s/def ::opts (s/* (s/cat :opt keyword? :val boolean?)))
  (s/conform ::opts [:silent? false :verbose true])
  (s/valid? ::opts [:silent? false :verbose true])

  (s/def ::config (s/*
                   (s/cat :prop string?
                          :val  (s/alt :s string? :b boolean?))))
  (s/conform ::config ["-server" "foo" "-verbose" true "-user" "joe"])

  (s/describe ::seq-of-keywords)
  (* keyword?)

)
