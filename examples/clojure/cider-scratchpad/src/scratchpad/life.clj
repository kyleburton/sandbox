(ns scratchpad.life
  (:require
   ;; [clojure.tools.logging :as log]
   ;; [clojure.java.io       :as io]
   ;; [schema.core           :as s]
   [clojure.test          :refer :all]
   ;; [scratchpad.core       :refer :all]
   [clojure.string        :as string]))

;; Conway's Game of Life

(def ALIVE \o)
(def DEAD \space)

(defn new-board [xx yy]
  {::x          xx
   ::y          yy
   ::generation 0
   ::grid       (for [_ (range yy)]
                  (make-array Character/TYPE xx))
   ::alt-grid   (for [_ (range yy)]
                  (make-array Character/TYPE xx))})

(defn grid-get [grid xx yy]
  (aget ^chars (nth grid yy) xx))

(defn grid-set! [grid xx yy ^Character val]
  (aset ^chars (nth grid yy) xx val))

(defn board->randomize [board rndf]
  (let [grid     (::alt-grid board)
        alt-grid (::grid board)]
    (doseq [^chars row grid]
      (dotimes [xx (::x board)]
        (let [^Character newv (if (rndf) ALIVE DEAD)]
          (aset row xx newv))))
    (assoc
     board
     ::grid       grid
     ::alt-grid   alt-grid
     ::generation 1)))

(defn board->string [board]
  (->>
   board
   ::grid
   (map #(string/join "" %))
   (string/join "\n")))

;; (rem (+ 0 -1 8) 8)
(defn live-neighbor-count [grid xx xdim yy ydim]
  (+
   (if (= ALIVE (grid-get grid (rem (+ xx -1 xdim) xdim) (rem (+ yy -1 ydim) ydim))) 1 0)
   (if (= ALIVE (grid-get grid (rem (+ xx -1 xdim) xdim) (rem (+ yy  0 ydim) ydim))) 1 0)
   (if (= ALIVE (grid-get grid (rem (+ xx -1 xdim) xdim) (rem (+ yy  1 ydim) ydim))) 1 0)

   (if (= ALIVE (grid-get grid (rem (+ xx  0 xdim) xdim) (rem (+ yy -1 ydim) ydim))) 1 0)
   ;; (if (= ALIVE (grid-get grid (rem (+ xx  0 xdim)) (rem (+ yy  0 ydim)))) 1 0)
   (if (= ALIVE (grid-get grid (rem (+ xx  0 xdim) xdim) (rem (+ yy  1 ydim) ydim))) 1 0)

   (if (= ALIVE (grid-get grid (rem (+ xx  1 xdim) xdim) (rem (+ yy -1 ydim) ydim))) 1 0)
   (if (= ALIVE (grid-get grid (rem (+ xx  1 xdim) xdim) (rem (+ yy  0 ydim) ydim))) 1 0)
   (if (= ALIVE (grid-get grid (rem (+ xx  1 xdim) xdim) (rem (+ yy  1 ydim) ydim))) 1 0)))

(defn board->next-generation [board]
  (let [new-grid (::alt-grid board)
        old-grid (::grid board)
        xdim     (::x board)
        ydim     (::y board)]
    (dotimes [xx xdim]
      (dotimes [yy ydim]
        (let [alive?     (= ALIVE (grid-get old-grid xx yy))
              live-count (live-neighbor-count old-grid xx xdim yy ydim)]
          (cond
            (and alive? (= live-count 2))
            (grid-set! new-grid xx yy ALIVE)

            (and alive? (= live-count 3))
            (grid-set! new-grid xx yy ALIVE)

            (and (not alive?) (= live-count 3))
            (grid-set! new-grid xx yy ALIVE)

            :else
            (grid-set! new-grid xx yy DEAD)))))
    (assoc
     board
     ::grid       new-grid
     ::alt-grid   old-grid
     ::generation (inc (::generation board)))))

(defn pboard->next-generation [board]
  (let [new-grid (::alt-grid board)
        old-grid (::grid board)
        xdim     (::x board)
        ydim     (::y board)]
    (doall
     (pmap
      (fn [yy ^chars old-row ^chars new-row]
        (dotimes [xx xdim]
          (let [alive?     (= ^Character ALIVE (aget old-row xx))
                live-count (live-neighbor-count old-grid xx xdim yy ydim)]
            (cond
              (and alive? (= live-count 2))
              (aset new-row xx ^Character ALIVE)

              (and alive? (= live-count 3))
              (aset new-row xx ^Character ALIVE)

              (and (not alive?) (= live-count 3))
              (aset new-row xx ^Character ALIVE)

              :else
              (aset new-row xx ^Character DEAD)))
          nil))
      (range ydim)
      old-grid
      new-grid))
    (assoc
     board
     ::grid       new-grid
     ::alt-grid   old-grid
     ::generation (inc (::generation board)))))

(comment
  (board->string
   (board->randomize
    (new-board 8 8)
    (fn [] (= 0 (rand-int 2)))))
  (->
   (new-board 10 10))

  (let [board (board->randomize
               (new-board 16 16)
               (fn [] (= 0 (rand-int 2))))]
    (loop [board       board
           generations 10]
      (if (zero? generations)
        board
        (do
          (.println System/out "==========")
          (.println System/out (board->string board))
          (recur (board->next-generation board)
                 (dec generations))))))

  )

(defn parse-args [args [def-xdim def-ydim def-generations]]
  (cond
    (= 1 (count args))
    [(Integer/parseInt (nth args 0))
     def-ydim
     def-generations]

    (= 2 (count args))
    [(Integer/parseInt (nth args 0))
     (Integer/parseInt (nth args 1))
     def-generations]

    (= 3 (count args))
    [(Integer/parseInt (nth args 0))
     (Integer/parseInt (nth args 1))
     (Integer/parseInt (nth args 2))]

    :else
    [def-xdim def-ydim def-generations]))

(defn -main [& args]
  (let [[xdim ydim generations] (parse-args args [60 40 1000])
        board (board->randomize
               (new-board xdim ydim)
               (fn [] (= 0 (rand-int 2))))]
    (loop [board       board
           generation  1
           generations generations]
      (if (zero? generations)
        board
        (do
          ;; (.print System/out "\u033[2J")
          ;; (.println System/out "\u001B[0m")
          (.println System/out "\u001B[2J")
          (.println System/out (board->string board))
          (.print System/out "===== ")
          (.print System/out (format "[%d %d] g=%d" xdim ydim generation))
          (recur (pboard->next-generation board)
                 (inc generation)
                 (dec generations)))))
    (.println System/out "DONE")))


;; lein run -m scratchpad.life $COLUMNS $LINES 9999
