(ns game-of-life.views
  (:require
   [re-frame.core :as re-frame]
   [game-of-life.styles :as styles]
   [game-of-life.config :as config]
   [game-of-life.events :as events]
   [game-of-life.routes :as routes]
   [game-of-life.subs :as subs]
   re-frame.db))

;; https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

(def cell-size 10)
(def line-size 1)

(defn draw-cell [ctx yy xx]
  (let [cell-size 10
        yy        (+ yy (* cell-size yy))
        xx        (+ xx (* cell-size xx))]
    (js/console.log "[INFO|game-of-life.views/draw-cell]: [%o %o] => [%o %o]" yy xx
                    (+ cell-size yy)
                    (+ cell-size xx))
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

(defn test-grid []
  (let [grid   (-> re-frame.db/app-db deref :grid)
        height (:height grid)
        width  (:width grid)
        canvas (.getElementById js/document "grid")
        ctx    (.getContext canvas "2d")]
    (js/console.log "[INFO|game-of-life.views/test-grid]: height=%o; width=%o" height width)
    ;; (set! (.-fillStyle ctx) "rgb(0,0,0)")
    ;; (.beginPath ctx)
    ;; (.rect ctx 0 0 height width)
    ;; (.fill ctx)
    (.clearRect ctx 0 0 height width)
    (set! (.-fillStyle ctx) "rgb(255,255,255)")
    (js/console.log "[INFO|game-of-life.views/test-grid]: fillStyle=%o" (.-fillStyle ctx))
    (doseq [yy (range height)
            xx (range width)
            :let [cell (aget (:cells grid) yy xx)]]
      (when (= 1 cell)
        (js/console.log "[INFO|game-of-life.views/test-grid]: filling [%o %o]" xx yy)
        (.beginPath ctx)
        (draw-cell ctx yy xx)
        (.fill ctx)))))

(comment
  (test-grid)
  )

(defn render-grid []
  (let [grid (re-frame/subscribe [::subs/grid])]
    (js/console.log "[INFO|game-of-life.views/render-grid]: grid=%o" @grid)
    [:canvas {:width  (+ line-size (* (+ cell-size line-size) (:width @grid)))
              :height (+ line-size (* (+ cell-size line-size) (:height @grid)))
              :id     "grid"
              :style  {:background "#000"}}]))

;; home
(defn home-panel []
  [:div
   [:h1
    {:class (styles/level1)}
    ;; (str "Hello from " @name ". This is the Home Page."" Git version " config/version)
    "Game of Life"]

   [:pre
    "DONE: init db w/defaults and blank grid\n"
    "DONE: render grid\n"
    "TODO: controls to change grid height & width\n"
    "TODO: controls to generate a random board\n"
    "TODO: controls to save a board state\n"
    "TODO: controls to edit a board\n"
    "TODO: controls to single-step; start and pause simulation\n"]

   (render-grid)

   [:div {:style {:height "1em"}}] ;; blank vertical spacing

   [:div
    [:a {:on-click #(re-frame/dispatch [::events/navigate :about])}
     "go to About Page"]]
   ])

(defmethod routes/panels :home-panel [] [home-panel])

;; about

(defn about-panel []
  [:div
   [:h1 "This is the About Page."]

   [:div
    [:a {:on-click #(re-frame/dispatch [::events/navigate :home])}
     "go to Home Page"]]])

(defmethod routes/panels :about-panel [] [about-panel])

;; main

(defn main-panel []
  (let [active-panel (re-frame/subscribe [::subs/active-panel])]
    (routes/panels @active-panel)))
