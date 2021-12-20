(ns hands-on-clojure.core
  (:import
   [org.jsoup Jsoup]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!")
  :status-is-go)

(defn bar
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))



(defn pmatch [[head & tail]]
  (println "Head: " head)
  (println "Tail: " tail))

(defn mmatch []
  (let [[a b c d] (.split "1,2,3,4" ",")
        {:keys [this that other]} {:this 2134 :that :stuff :other "banana"}]
    (println "a: " a)
    (println "b: " b)
    (println "c: " c)
    (println "d: " d)
    (println "this: " this)
    (println "that" that)
    (println "other" other)))

(defn -main [& args]
  (let [[cmd & args] args]
    (cond
     (= cmd "pmatch")
     (pmatch args)

     (= cmd "mmatch")
     (mmatch)

     :otherwise
     (println "sorry, don't know what to do with " cmd))))
