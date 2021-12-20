(ns scratchpad.life)

;; rules: goo.gl/wn7hQ2
;; => https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Rules


;; cell
;; - state :- boolean


(defn xtest [descr assertion]
  (if-not assertion
    (throw (RuntimeException. (format "Error: %s failed!" descr)))
    true))

(defn cell-is-alive? [] true)
(defn cell-is-dead?  [] false)

(defn create-live-cell []
  {:type      :cell
   :is-alive? cell-is-alive?})

(comment

  (= (create-live-cell)
     (create-live-cell))
)


(defn create-dead-cell []
  {:type      :cell
   :is-alive? cell-is-dead?})

(defn create-empty-world [rows cols]
  {:type :world
   :rows rows
   :cols cols
   :cells (mapv (fn [c]
                  (mapv (fn [c] (create-dead-cell))
                        (range rows)))
                (range cols))})


(defn create-world [state]
  {:type :world
   :rows (count state)
   :cols (count (get state 0))
   :cells state
   }

  ;; jskulski@gmail.com jon skulski


  ;; discussion is conflating primitivs vs domain with the idea of
  ;; types, offering that primitives can't also represent meaning in
  ;; the domain .. values can do this -- Haskell and other strong type
  ;; systems allow for value types that do have meaning in the domain
  ;; .. values!

  ;; some of the discussion ... I lost this b/c I was talking :(

  ;; 
  )


(comment

  (->
   (create-world)
   :next
   :next
   :next
   :into-row
   :next
   :next)
  ;; => this would get you to the cell [3, 2] w/o any primitive numbers in the code

)


(defn fetch-cell [world row col]
   (get (get (:cells world) row) col))

;; (get [1 2 3] 0)
;; (def row 5)
;; (def col 5)
;; (def rows 10)
;; (def cols 10)
;   static final Object rows = 10;
;; (range 10)
;; (def world (create-world 10 10))
(defn kill-cell [_]
  (create-dead-cell))

(defn revive-cell [_]
  (create-live-cell))

(comment
  (run-all-tests)

)

(defn run-all-tests []

  (xtest "can create a live cell"
         ((:is-alive? (create-live-cell))))

  (xtest "can create a dead cell" 
         (not ((:is-alive? (create-dead-cell)))))

  (xtest "can create a empty world"
         (create-empty-world 10 10))

  (xtest "can create a world with initial state"
         (let [world (create-world [[(create-dead-cell) (create-live-cell)]
                                    [(create-dead-cell) (create-live-cell)]])]
           (assert (= 2 (:rows world)))
           (assert (= 2 (:cols world)))
           #_(assert (= 2 (num-live-cells world)))
           #_(assert (= 2 (num-dead-cells world)))))
  
  (xtest "can fetch a cell"
         (= (create-dead-cell) (fetch-cell (create-world 10 10) 5 5)))

  (xtest "can kill a cell"
         (= (create-dead-cell) (kill-cell (create-live-cell))))

  (xtest "can bring a cell to life"
         (= (create-live-cell) (revive-cell (create-dead-cell))))

  
  )
