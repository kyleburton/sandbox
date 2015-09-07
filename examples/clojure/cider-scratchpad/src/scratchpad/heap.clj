(ns scratchpad.heap
  (:require
   [clojure.tools.logging   :as log]
   [clojure.core            :as core]))

;; Binomoial heap and Fibonacci heap

(defprotocol Heap
  (insert     [h v])
  (insert-all [h vs])
  (peek       [h])
  (pop        [h])
  (walk       [h f]))

(defn make-heap []
  {:elts      []
   :heap-size 0})

(defn parent-idx [idx]
  (int (/ (- idx 1) 2)))

(defn left-idx [idx]
  (+ 1 (* idx 2)))

(defn right-idx [idx]
  (+ 2 (* idx 2)))

(defn left [elts idx]
  (get elts (left-idx idx)))

(defn right [elts idx]
  (get elts (right-idx idx)))

(defn swap [elts ii jj]
  (let [v    (get elts ii)
        elts (assoc elts ii (get elts jj))]
    (assoc elts jj v)))

(defn rebalance [elts cmp]
  (loop [elts elts
         ii   (dec (count elts))]
    (cond
     (zero? ii)
     elts

     (not (cmp (get elts (parent-idx ii))
               (get elts ii)))
     (recur (swap elts ii (parent-idx ii))
            (parent-idx ii))

     :else
     elts)))

(defn heapify-down [pos elts cmp]
  (let [e    (get elts pos)
        lidx (left-idx pos)
        ridx (right-idx pos)
        size (count elts)
        sidx (if (and
                  (< lidx size)
                  (not (cmp e (get elts lidx))))
               lidx
               pos)
        sidx (if (and
                  (< ridx size)
                  (not (cmp e (get elts ridx))))
               ridx
               sidx)]
    (if (= pos sidx)
      elts
      (recur sidx (swap elts pos sidx) cmp))))

(defn walk-elts [elts pos depth f]
  (when (< pos (count elts))
    (f pos depth (get elts pos))
    (walk-elts
     elts
     (left-idx pos)
     (inc depth)
     f)
    (walk-elts
     elts
     (right-idx pos)
     (inc depth)
     f)))

(defrecord
    BinHeap [op]
  Heap
  (insert [h v]
    (let [elts (rebalance (conj (:elts h []) v) op)]
      (assoc h :elts elts)))
  (insert-all [h vs]
    (loop [h        h
           [v & vs] vs]
      (cond
       (nil? v)
       h

       :else
       (recur (insert h v)
              vs))))
  (peek [h]
    (-> h :elts first))
  (pop  [h]
    (let [lval (-> h :elts core/peek)
          elts (-> h
                   :elts
                   core/pop)]
      (assoc
          h
        :elts (if (empty? elts)
                []
                (heapify-down 0 (assoc elts 0 lval) (:op h))))))
  (walk [h f]
    (walk-elts (:elts h) 0 0 f)))

(defn heap->pairs [h & [pos]]
  (let [pos (or pos 0)]
    (when (< pos (-> h :elts count))
      (let [elts (:elts h)
            e    (get elts pos)
            le   (left elts  pos)
            re   (right elts pos)
            pairs []
            pairs (if le
                    (concat
                     pairs
                     [[e le]]
                     (heap->pairs h (left-idx pos)))
                    pairs)
            pairs (if re
                    (concat
                     pairs
                     [[e re]]
                     (heap->pairs h (right-idx pos)))
                    pairs)]
        pairs))))

(defn pairs->digraph [pairs]
  (let [sb (StringBuilder.)]
    (.append sb "digraph Heap {\n")
    (doseq [[l r] pairs]
      (.append sb (str "  \"" l "\" -> \"" r "\";\n")))
    (.append sb "}")))

(comment
  (spit
   "h2.dot"
   (->
    (BinHeap. >=)
    (insert-all [19 36 17 3 25 1 2 7 100 109 18])
    heap->pairs
    pairs->digraph))

  (->
   (BinHeap. >=)
   (insert-all [19 36 17])
   pop)
  
  [36 19 17]

  (->
   (BinHeap. >=)
   (insert-all [100 19 36 17 3 25 1 2 7])
   (walk (fn [pos depth elt] (log/infof "pos=%s depth=%s elt=%s" pos depth elt))))


  (->
   (BinHeap. >=)
   (insert-all [19 36 17 3 25 1 2 7 100 109])
   pop
   :elts)
  [100 36 17 25 19 1 2 3 7]



  )

(defn is-valid-heap? [h]
  ;;    the value in each node is the maximum within its branch
  (loop [[[l r] & pairs] (heap->pairs h)]
    (cond
     (not l)
     :ok
     
     ((:op h) l r)
     (recur pairs)
     
     :otherwise
     :fail)))





(comment

  (->
   (BinHeap. >=)
   (insert-all [19 36 17 3 25 1 2 7 100 109])
   is-valid-heap?)

  (spit
   "h4.dot"
   (->
    (BinHeap. <=)
    (insert-all [19 36 17 3 25 1 2 7 100 109 172])
    heap->pairs
    pairs->digraph))

  )
