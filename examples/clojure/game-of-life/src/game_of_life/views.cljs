(ns game-of-life.views
  (:require
   [re-frame.core       :as re-frame]
   [game-of-life.styles :as styles]
   [game-of-life.config :as config]
   [game-of-life.events :as events]
   [game-of-life.routes :as routes]
   [game-of-life.subs   :as subs]
   [game-of-life.canvas :as canvas]
   [reagent.core        :as reagent]
   re-frame.db))

;; https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
(defonce chicken (atom nil))

(defn on-click-canvas [evt]
  (let [grid        (js/document.getElementById "grid")
        offset-left (.-offsetLeft grid)
        client-left (.-clientLeft grid)
        elm-left    (+ offset-left client-left)
        offset-top  (.-offsetTop grid)
        client-top  (.-clientTop grid)
        elm-top     (+ offset-top client-top)
        xx          (- (.-pageY evt) elm-top)
        yy          (- (.-pageX evt) elm-left)
        cell-xx     (int (/ xx (+ canvas/line-size canvas/cell-size)))
        cell-yy     (int (/ yy (+ canvas/line-size canvas/cell-size)))]
    (js/console.log "[INFO|game-of-life.views/on-click-canvas]: %o grid.offsetLeft=%o; grid.clientLeft=%o; grid.offsetTop=%o; grid.clientTop=%o; cell=%o" [yy xx]
                    (.-offsetLeft grid)
                    (.-clientLeft grid)
                    (.-offsetTop grid)
                    (.-clientTop grid)
                    [cell-yy cell-xx])
    (re-frame/dispatch [::events/toggle-cell cell-yy cell-xx])
    (js/setTimeout (fn [] (canvas/render-gol-canvas)) 0)))

(defn render-grid []
  (let [grid (re-frame/subscribe [::subs/grid])]
    [:canvas {:width    (+ canvas/line-size (* (+ canvas/cell-size canvas/line-size) (:width @grid)))
              :height   (+ canvas/line-size (* (+ canvas/cell-size canvas/line-size) (:height @grid)))
              :on-click on-click-canvas
              :id       "grid"
              :style    {:background "#000"}}]))

(defn render-grid-controls []
  (let [grid         (re-frame/subscribe [::subs/grid])
        ui-state     (re-frame/subscribe [::subs/ui-state])
        can-go-back? (-> grid deref :prev-cells nil? not)]
    (js/console.log "[INFO|game-of-life.views/render-grid-controls]: can-go-back?=%o" can-go-back?)
    [:div
     [:div "Generation: " (:generation @grid)]
     [:button {:on-click #(canvas/render-gol-canvas)} "Draw"]
     [:button {:on-click (fn []
                           (re-frame/dispatch [::events/gol-go-back])
                           (js/setTimeout (fn [] (canvas/render-gol-canvas)) 0))
               :disabled (not can-go-back?)}
      "Back"]
     [:button {:on-click (fn []
                           (re-frame/dispatch [::events/gol-step])
                           (js/setTimeout (fn [] (canvas/render-gol-canvas)) 0))}
      "Step"]
     (if (-> ui-state deref :running)
       [:button {:on-click #(re-frame/dispatch [::events/gol-stop])}  "Stop"]
       [:button {:on-click #(re-frame/dispatch [::events/gol-start])} "Start"])
     [:button {:on-click (fn []
                           (re-frame/dispatch [::events/gol-new-random])
                           (js/setTimeout (fn [] (canvas/render-gol-canvas)) 0))}
      "Randomize"]
     [:button {:on-click (fn []
                           (re-frame/dispatch [::events/gol-reset])
                           (js/setTimeout (fn [] (canvas/render-gol-canvas)) 0))}
      "Reset"]
     [:span "Height"
      [:input {:type      "text"
               :value     (-> grid deref :height)
               :on-change (fn [evt]
                            (re-frame/dispatch [::events/gol-update-height evt (js/parseInt (-> evt .-target .-value))])
                            (js/setTimeout (fn [] (canvas/render-gol-canvas)) 0))
               :on-blur   (fn [evt]
                            (re-frame/dispatch [::events/gol-resize-matrix evt])
                            (js/setTimeout (fn [] (canvas/render-gol-canvas)) 0))}]]
     [:span "Width"
      [:input {:type      "text"
               :value     (-> grid deref :width)
               :on-change (fn [evt]
                            (re-frame/dispatch [::events/gol-update-width evt (js/parseInt (-> evt .-target .-value))])
                            (js/setTimeout (fn [] (canvas/render-gol-canvas)) 0))
               :on-blur   (fn [evt]
                            (js/setTimeout (fn [] (canvas/render-gol-canvas)) 0)
                            (re-frame/dispatch [::events/gol-resize-matrix evt]))}]]]))

;; home
(defn home-panel []
  [:div
   [:h1
    {:class (styles/level1)}
    ;; (str "Hello from " @name ". This is the Home Page."" Git version " config/version)
    "Game of Life"]

   (render-grid)
   (render-grid-controls)

   [:div {:style {:height "1em"}}] ;; blank vertical spacing
   ;; [:pre (js/JSON.stringify (-> grid deref (dissoc :cells) clj->js) nil 2)]
   ;; [:div {:style {:height "1em"}}] ;; blank vertical spacing


   [:div
    [:a {:on-click #(re-frame/dispatch [::events/navigate :about])}
     "go to About Page"]]])

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn document-key-up [evt]
  (re-frame/dispatch [::events/document-keyup evt]))

(defn gol-keyhandler []
  (reagent/create-class
   {:display-name "gol-keyhandler"
    :component-will-mount
    (fn [_component]
      ;; install the key handler
      (js/console.log "[INFO|game-of-life.views/sw-keyhandler]: installing key handler")
      (js/document.addEventListener "keyup" document-key-up false))
    :component-will-unmount
    (fn [_component]
      ;; remove the key handler
      (js/console.log "[INFO|game-of-life.views/sw-keyhandler]: removing key handler")
      (js/document.removeEventListener "keyup" document-key-up false))
    :reagent-render
    (fn []
      [:span {:key "gol-keyhandler" :style {:display "none"}}])}))
