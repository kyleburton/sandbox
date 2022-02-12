(ns game-of-life.canvas
  (:require
   re-frame.db))


(def cell-size 10)
(def line-size 1)

(defn draw-cell [ctx yy xx]
  (let [cell-size 10
        yy        (+ yy (* cell-size yy))
        xx        (+ xx (* cell-size xx))]
    (.rect ctx yy xx cell-size cell-size)))

(comment
  (let [canvas (.getElementById js/document "grid")
        ctx    (.getContext canvas "2d")]
    (.clearRect ctx 0 0 (* 11 100) (* 11 100))
    (set! (.-fillStyle ctx) "rgb(255,255,255)")
    (.beginPath ctx)
    (draw-cell ctx 0 0)
    (draw-cell ctx 1 1)
    (draw-cell ctx 2 2)
    (draw-cell ctx 3 3)
    (.fill ctx))

  )

(defn render-gol-canvas []
  (let [grid      (-> re-frame.db/app-db deref :grid)
        height    (:height grid)
        width     (:width grid)
        canvas    (.getElementById js/document "grid")
        ctx       (.getContext canvas "2d")
        num-alive (atom 0)]
    (js/console.log "[INFO|game-of-life.views/test-grid]: height=%o; width=%o" height width)
    (set! (.-fillStyle ctx) "rgb(0,0,0)")
    (.clearRect ctx
                0
                0
                (* (+ line-size cell-size) (:height grid))
                (* (+ line-size cell-size) (:width grid)))
    (set! (.-fillStyle ctx) "rgb(255,255,255)")
    (js/console.log "[INFO|game-of-life.views/test-grid]: fillStyle=%o" (.-fillStyle ctx))
    (doseq [yy   (range height)
            xx   (range width)
            :let [cell (aget (:cells grid) yy xx)]]
      (when (= 1 cell)
        (swap! num-alive inc)
        (.beginPath ctx)
        (draw-cell ctx yy xx)
        (.fill ctx)))
    (let [total (* height width)]
      (js/console.log "[INFO|game-of-life.views/render-gol-canvas]: %ox%o (%o) num-alive=%o; num-dead=%o" height width total @num-alive (- total @num-alive)))))

(comment
  (render-gol-canvas)
  (js/setTimeout #(render-gol-canvas) 1000)
  )
