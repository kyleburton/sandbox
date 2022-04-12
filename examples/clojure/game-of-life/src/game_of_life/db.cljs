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

(defn reset-grid [grid]
  (let [height    (:height grid)
        width     (:width grid)
        cells     (or (:cells grid) (make-array nil height width))]
    (doseq [yy (range height)
            xx (range width)]
      (aset cells yy xx nil))
    (assoc grid
           :cells cells
           :generation 0)))

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
    (assoc
     (dissoc grid-specs :prev-cells)
     :cells      cells
     :generation 0)))

(defn clone-cells [cells]
  (let [new-cells (js/Array.from cells)]
    (doseq [ii (range (alength cells))]
      (aset new-cells ii (js/Array.from (aget cells ii))))
    new-cells))

(defn resize-cells [cells new-height new-width]
  (let [new-cells  (make-array nil new-height new-width)
        old-height (alength cells)
        old-width  (alength (aget cells 0))]
    (js/console.log "[INFO|game-of-life.db/resize-cells]: resizing [%o, %o] => [%o, %o]"
                    old-height old-width
                    new-height new-width)
    (doseq [yy (range   (min new-height old-height))]
      (doseq [xx (range (min new-width  old-width))]
        (aset new-cells yy xx (aget cells yy xx))))
    (js/console.log "[INFO|game-of-life.db/resize-cells]: completed resizing to [%o, %o]"
                    new-height new-width)
    new-cells))

(comment
  (let [old-matrix (make-array nil 3 3)
        new-matrix (resize-cells old-matrix 4 4)]
    (assert
     (= 3 (alength old-matrix)))
    (assert
     (= 3 (alength (aget old-matrix 0))))
    (assert
     (= 4 (alength new-matrix)))
    (assert
     (= 4 (alength (aget new-matrix 0)))))

  (let [old-matrix (make-array nil 4 4)
        new-matrix (resize-cells old-matrix 3 3)]
    (assert
     (= 4 (alength old-matrix)))
    (assert
     (= 4 (alength (aget old-matrix 0))))
    (assert
     (= 3 (alength new-matrix)))
    (assert
     (= 3 (alength (aget new-matrix 0)))))


  )

(defn resize-matrix! [db]
  (let [old-cells (-> db :grid :cells)
        new-cells (resize-cells old-cells (-> db :grid :height) (-> db :grid :width))]
    (assoc-in db [:grid :cells] new-cells)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn width
  ([]
   (-> re-frame.db/app-db deref :grid :width))
  ([db]
   (-> db :grid :width))
  ([db val]
   (->
    db
    (assoc-in [:grid :width] val)
    #_(assoc-in [:grid :cells]
                (resize-cells
                 (-> db :grid :cells)
                 (-> db :grid :height)
                 val)))))

(defn height
  ([]
   (-> re-frame.db/app-db deref :grid :height))
  ([db]
   (-> db :grid :height))
  ([db val]
   (->
    db
    (assoc-in [:grid :height] val)
    #_(assoc-in [:grid :cells]
                (resize-cells
                 (-> db :grid :cells)
                 val
                 (-> db :grid :width))))))

(def neighbor-index-offsets
[[-1 -1]
[-1  0]
[-1  1]

[0 -1]
[0  1]

[1 -1]
[1  0]
[1  1]])

(defn neighbor-coords [cells height width yy xx]
  (map
   (fn [[yoff xoff]]
     (let [maxy height
           maxx width
           nyy (+ yy yoff)
           nyy (cond
                 (= -1 nyy)   (count cells)
                 (= maxy nyy)  0
                 :else        nyy)
           nxx (+ xx xoff)
           nxx (cond
                 (= -1 nxx)   (count cells)
                 (= maxx nxx)  0
                 :else        nxx)]
       [nyy nxx]))
   neighbor-index-offsets))

(defn neighbors [cells height width yy xx]
  (map
   (fn [[yoff xoff]]
     (let [maxy height
           maxx width
           nyy  (+ yy yoff)
           nyy  (cond
                  (= -1 nyy)   (dec (count cells))
                  (= maxy nyy) 0
                  :else        nyy)
           nxx  (+ xx xoff)
           nxx  (cond
                  (= -1 nxx)   (dec (count cells))
                  (= maxx nxx) 0
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
      (neighbors cells 3 3 1 1))
    '(0 1 2 3 5 6 7 8)))

  )

(defn count-num-live-neighbors [height width cells yy xx]
  (count
   (filter #(= 1 %)
           (neighbors cells height width yy xx))))

(defn next-grid [grid]
  (let [cells      (:cells grid)
        height     (alength cells)
        width      (alength (get cells 0))
        prev-cells (:prev-cells grid)
        new-cells  (cond ;; if prev-cells is not the same size as cells, we can't use it
                     (and prev-cells
                          (= (alength cells)
                             (alength prev-cells))
                          (= (alength (aget cells 0))
                             (alength (aget prev-cells 0))))
                     prev-cells

                     :else
                     (make-array nil height width))]
    (doseq [yy   (range height)
            xx   (range width)
            :let [cell               (aget cells yy xx)
                  num-live-neighbors (count-num-live-neighbors height width cells yy xx)
                  alive? (= 1 cell)
                  dead?  (not alive?)]]
      (cond
        ;; Any live cell with two or three live neighbours survives.
        (and
         alive?
         (or (= 2 num-live-neighbors)
             (= 3 num-live-neighbors)))
        (aset new-cells yy xx 1)

        ;; Any dead cell with three live neighbours becomes a live cell.
        (and
         dead?
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prebuilt objects
(defn strings->object [strings]
  nil)

(def objects
  {:glider (strings->object
            "XXX"
            "  X"
            " X ")})
