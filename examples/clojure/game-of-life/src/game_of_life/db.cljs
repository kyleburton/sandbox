(ns game-of-life.db
  (:require
   re-frame.db))

;; (def default-db
;;   {:name "re-frame"
;;    :grid {:cells      []
;;           :height     100
;;           :width      100
;;           :generation 0}})

(def grid-defaults {:height     25
                    :width      25
                    :generation 0})

(comment
  (make-array nil 5 5)

  )

(defn generate-grid [grid-specs]
  (let [height    (:height grid-specs)
        width     (:width grid-specs)
        cells     (make-array nil height width)
        num-alive (atom 0)
        num-dead  (atom 0)]
    (doseq [yy   (range height)
            xx   (range width)
            :let [alive? (>= (rand 2) 1)]]
      (if alive?
        (do
          (aset cells yy xx 1)
          (swap! num-alive inc))
        (do
          (aset cells yy xx nil)
          (swap! num-dead inc))))
    (js/console.log "[INFO|game-of-life.db/generate-grid]: %ox%o (%o) grid %o alive %o dead"
                    height
                    width
                    (* height width)
                    @num-alive
                    @num-dead)
    (assoc (dissoc grid-specs :prev-cells) :cells cells)))

(defn generate-default-db []
  {:name     "Game of Life"
   :grid     (generate-grid grid-defaults)
   :ui-state {:running false}})

(defn start [db]
  (assoc-in db [:ui-state :running] true))

(defn stop [db]
  (assoc-in db [:ui-state :running] false))

(defn grid
  ([]
   (-> re-frame.db/app-db deref :grid))
  ([db]
   (:grid db))
  ([db new-board]
   (assoc db :grid new-board)))

;; todo: need to resize the cells to match the new width
(defn width
  ([]
   (-> re-frame.db/app-db deref :grid :width))
  ([db]
   (-> db :grid :width))
  ([db val]
   (assoc-in db [:grid :width] val)))

;; todo: need to resize the cells to match the new width
(defn height
  ([]
   (-> re-frame.db/app-db deref :grid :height))
  ([db]
   (-> db :grid :height))
  ([db val]
   (assoc-in db [:grid :height] val)))

(def neighbor-index-offsets
  [[-1 -1]
   [-1  0]
   [-1  1]

   [0 -1]
   [0  1]

   [1 -1]
   [1  0]
   [1  1]])

(defn neighbor-coords [cells yy xx]
  (map
   (fn [[yoff xoff]]
     (let [max (alength cells)
           nyy (+ yy yoff)
           nyy (cond
                 (= -1 nyy)   (count cells)
                 (= max nyy)  0
                 :else        nyy)
           nxx (+ xx xoff)
           nxx (cond
                 (= -1 nxx)   (count cells)
                 (= max nxx)  0
                 :else        nxx)]
       [nyy nxx]))
   neighbor-index-offsets))

(defn neighbors [cells yy xx]
  (map
   (fn [[yoff xoff]]
     (let [max (alength cells)
           nyy (+ yy yoff)
           nyy (cond
                 (= -1 nyy)   (dec (count cells))
                 (= max nyy)  0
                 :else        nyy)
           nxx (+ xx xoff)
           nxx (cond
                 (= -1 nxx)   (dec (count cells))
                 (= max nxx)  0
                 :else        nxx)]
       (aget cells nyy nxx)))
   neighbor-index-offsets))

(comment

  (assert
   (=
    (let [cells (make-array nil 3 3)]
      (doseq [yy (range 3)
              xx (range 3)]
        (aset cells yy xx (+ (* yy 3) xx)))
      (neighbors cells 1 1))
    '(0 1 2 3 5 6 7 8)))

  )

(defn count-num-live-neighbors [cells yy xx]
  (count
   (filter #(= 1 %)
           (neighbors cells yy xx))))

(defn next-grid [grid]
  (let [height    (:height grid)
        width     (:width grid)
        cells     (:cells grid)
        new-cells (or (:prev-cells grid) (make-array nil height width))]
    (doseq [yy   (range height)
            xx   (range width)
            :let [cell               (aget cells yy xx)
                  num-live-neighbors (count-num-live-neighbors cells yy xx)]]
      ;; (js/console.log "[INFO|game-of-life.db/next-grid]: [%o %o]:%s num-live-neighbors=%o" yy xx (if (= 1 cell) "alive" "dead") num-live-neighbors)
      (cond
        ;; Any live cell with two or three live neighbours survives.
        (and
         (= 1 cell)
         (or (= 2 num-live-neighbors)
             (= 3 num-live-neighbors)))
        (aset new-cells yy xx 1)

        ;; Any dead cell with three live neighbours becomes a live cell.
        (and
         (nil? cell)
         (= 3 num-live-neighbors))
        (aset new-cells yy xx 1)

        ;; All other live cells die in the next generation. Similarly, all other dead cells stay dead.
        :else
        (aset new-cells yy xx nil)))
    (assoc grid
           :prev-cells cells
           :cells      new-cells
           :generation (inc (:generation grid)))))


(defn go-back [db]
  (let [grid       (:grid db)
        prev-cells (:prev-cells grid)
        grid       (dissoc grid :prev-cells)
        grid       (assoc grid :cells prev-cells)]
    (assoc db :grid grid)))

(defn clone-cells [cells]
  (let [new-cells (js/Array.from cells)]
    (doseq [ii (range (alength (aget cells 0)))]
      (aset new-cells ii (js/Array.from (aget cells ii))))
    new-cells))

(comment

  (let [cells (make-array nil 3 3)]
    (doseq [yy (range 3)
            xx (range 3)]
      (aset cells yy xx (+ (* 3 yy) xx)))
    (clone-cells cells))

  )

(defn toggle-cell [db yy xx]
  (let [cells     (-> db :grid :cells)
        new-cells (clone-cells cells)
        cell      (aget new-cells yy xx)
        new-cell  (if (= 1 cell)
                    nil
                    1)]
    (aset new-cells yy xx new-cell)
    (assoc-in db [:grid :cells]
              new-cells)))
