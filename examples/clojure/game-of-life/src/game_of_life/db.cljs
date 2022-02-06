(ns game-of-life.db)

;; (def default-db
;;   {:name "re-frame"
;;    :grid {:cells      []
;;           :height     100
;;           :width      100
;;           :generation 0}})

(defn generate-grid [grid-specs]
  (let [height (:height grid-specs)
        width  (:width grid-specs)
        cells  (make-array height)]
    (doseq [row (range height)]
      (aset cells row (make-array width)))
    (doseq [yy (range height)
            xx (range width)
            :let [filled? (>= (rand 2) 1)]]
      (if filled?
        (aset cells yy xx 1)
        (aset cells yy xx 0)))
    (assoc grid-specs :cells cells)))

(defn generate-default-db []
  {:name "Game of Life"
   :grid (generate-grid {:height     100
                         :width      100
                         :generation 0})})
