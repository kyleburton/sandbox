(ns scratchpad.life
  (:require
   [clojure.tools.logging :as log]
   [clojure.java.io       :as io]
   [schema.core           :as s]
   [clojure.test          :refer :all]
   [scratchpad.core       :refer :all]
   [clojure.string        :as string]))

;; game of life
;; constraints: no primivies

;; board is a sparse array
;;   as a map, the map's keys are rows
;;   each row is the set of cells (indexes) that are alive
(comment

  (string/join "\n" ["this" "tha" "other"])

  (contains? nil 1)
  )

(defn count-live-neighbors [board y x]
  (let [neighbor-offsets [[-1 -1] [-1 0] [-1 1]
                          [0 -1]         [0 1]
                          [1 -1] [1 0] [1 1]]]
    (reduce
     (fn [acc [yoff xoff]]
       (+ acc
          (if ???
            1
            0))))))

(defn print-viewport [board top left height width]
  ;; returns a string of x's where the cell is alive
  (string/join
   "\n"
   (map
    (fn [i]
      (apply
       str
       (map
        (fn [j] 
          (if (contains? (board i) j)
            "X"
            "."))
        (range left (+ left width)))))
    (range top (+ top height)))))

(comment

  (print-viewport {0 #{0 1 2}} 0 0 1 3)

  (print-viewport {0 #{0 1 2}} 0 0 1 1)

  )

(defn test-count-live-neighbors []
  (let [result (count-live-neighbors {0 #{0 1 2}} 0 1)]
    (if (not= result
              2)
      (throw (RuntimeException. (format "Error: expected 2 got: %s" result))))))

(defn test-print-viewport []
  (let [result (print-viewport {0 #{0 1 2}} 0 0 1 3)]
    (if (not= result
              "XXX")
      (throw (RuntimeException. (format "Error: expected 'xxx' got: %s" result)))))
  (let [result (print-viewport {0 #{0 1 2}} 0 0 1 1)]
    (if (not= result
              "X")
      (throw (RuntimeException. (format "Error: expected 'x' got: %s" result)))))
  (let [result (print-viewport {0 #{0 1 2}} 0 0 2 2)]
    (if (not= result
              "XX\n..")
      (throw (RuntimeException. (format "Error: expected '\nxx\n..\n' got: %s" result))))))


(do
  (test-print-viewport)
  (test-count-live-neighbors))
