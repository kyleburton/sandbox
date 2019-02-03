(ns scratchpad.life
  (:require
   [clojure.tools.logging         :as log]
   [clojure.core                  :as core]
   [clj-etl-utils.sequences       :as etl-seq]
   [clj-etl-utils.landmark-parser :as lp]
   [clojure.java.io               :as io]
   [clojure.string                :as string]
   [schema.core                   :as s]))


;; game of life
;; represent as a sparse data strcuture
;; map of rows to columns
;; rows are just indicies
;; cols are just a set of the cells that are alive

(defn make-board [dim live-cell-positions]
  (reduce
   (fn [board [xx yy]]
     (update-in board [xx] #(conj (or % #{}) yy)))
   {:dim dim}
   live-cell-positions))

(defn board->str [board [x1 y1] [x2 y2]]
  (string/join
   "\n"
   (map
    (fn [yy]
      (apply str
             (map
              (fn [xx]
                (if (-> board (get yy) (contains? xx))
                  "X"
                  "."))
              (range x1 (inc x2)))))
    (range y1 (inc y2)))))

(defn num-live-neighbors [board [xx yy]]
  ;; (-> board (get yy) (contains? xx))
  (loop [[[xoff yoff] & offsets] [[-1 1]  [0 1]  [1 1]
                                  [-1 0]         [1 0]
                                  [-1 -1] [0 -1] [1 -1]]
         acc 0]
    (cond
      (nil? xoff)
      acc
      
      (-> board (get (+ yy yoff)) (contains? (+ xx xoff)))
      (recur offsets (inc acc))
      
      :otherwise
      (recur offsets acc))))

(comment
  (num-live-neighbors (make-board 3 [[1 1]]) [3 0])

  )

(defn board->next-board [board]
  ;; NB: each cell's subsequent state can be calcualted in PARALLEL
  (let [dim (:dim board)]
    ;; compute from (dec dim) up to (inc dim) in both xx and yy
    ;; TODO: as an optimization we only need to look at
    ;;           (dec (min (keys board)))
    ;;       and (inc (max (keys board)))
    ;; as no cells outside of that range can be alive
    (let [xs   (-> board keys)
          minx (apply min xs)
          maxx (apply max xs)]
      )

    (reduce
     (fn [acc [xx yy]]
       (let [live-count (num-live-neighbors board [xx yy])
             alive?     (-> board (get yy) (contains? xx))]
         (cond
           ;; 1. Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
           (and alive? (< live-count 2))
           acc
           
           ;; 2. Any live cell with two or three live neighbours lives on to the next generation.
           (and alive? (or (= live-count 2)
                           (= live-count 3)))
           (update-in acc [yy] #(conj (or % #{}) xx))
           
           ;; 3. Any live cell with more than three live neighbours dies, as if by overpopulation.
           (and alive? (> live-count 3))
           acc
           
           ;; 4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
           (and (not alive?) (= live-count 3))
           (update-in acc [yy] #(conj (or % #{}) xx))

           :otherwise
           acc)))
     {:dim dim}
     (range (inc dim))
     (range (inc dim)))))

(comment
reduce
  (->
   (make-board 3 [[1 1]])
   board->next-board)
  
  (do
    (print "\n\n")
    (print (board->str (make-board 3 [[1 1]]) [0 0] [40 40])))

  )
