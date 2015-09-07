(ns scratchpad.scratch
  (:require
   [clojure.tools.logging   :as log]
   [clojure.core            :as core]
   [clj-etl-utils.sequences :as etl-seq]
   [scratchpad.heap         :as heap]
   [clojure.java.io         :as io])
  (:import
   [scratchpad.heap BinHeap]))


(defonce words-file "/usr/share/dict/words")
(def num-words (memoize (fn []
                          (with-open [rdr (io/reader words-file)]
                            (count (line-seq rdr))))))

(defn random-words [n]
  (with-open [rdr (io/reader words-file)]
    (doall
     (etl-seq/random-sample-seq
      (line-seq rdr)
      (num-words)
      n))))

(defn test-seq [tname op s]
  (spit
   (str tname ".dot")
   (->
    (BinHeap. op)
    (heap/insert-all s)
    heap/heap->pairs
    heap/pairs->digraph)))

(defn test-seq->heap [op s]
  (->
   (BinHeap. op)
   (heap/insert-all s)))

(comment

  (test-seq
   "h5"
   <= ;; >=
   (->>
    (repeatedly rand)
    (map #(* 10000 %))
    (map int)
    (take 25)))


  (test-seq
   "w1"
   (fn [a b]
     (= -1 (.compareToIgnoreCase a b)))
   (random-words 25))

  (test-seq
   "w2"
   (fn [a b]
     (not= -1 (.compareToIgnoreCase a b)))
   (random-words 25))


  )
