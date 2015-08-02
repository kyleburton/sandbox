(ns ^:figwheel-always tetris.core
    (:require
     [tetris.world   :as world]
     [tetris.view    :as view]
     [reagent.core   :as reagent :refer [atom]]
     [clojure.string :as string]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload
(defonce app-state
  (atom {:tick-interval    300
         :tick-interval-id nil}))

(defn set-tick-interval! [interval-ms]
  (swap! app-state assoc :tick-interval interval-ms))

;; (set-tick-interval! 500)

;; (defn hello-world []
;;   [:h1 (:text @app-state)])

;; (reagent/render-component [hello-world]
;;                           (. js/document (getElementById "app")))


;; (defn on-js-reload []
;;   ;; optionally touch your app-state to force rerendering depending on
;;   ;; your application
;;   (swap! app-state update-in [:__figwheel_counter] inc)
;;   )


(defn maybe-step [world f]
  (let [new-world (f world)]
    (if (world/valid-world? new-world)
      (reset! world/app-state new-world)
      world)))

(def codename
  {37 "LEFT"
   38 "UP"
   39 "RIGHT"
   40 "DOWN"
   32 "SPACE"})

(def action
  {"LEFT"  world/move-left
   "RIGHT" world/move-right
   "UP"    world/rotate
   "SPACE" world/rotate
   "DOWN"  world/drop-to-ground})

(defn handle-keydown [e]
  (when-not (:done @world/app-state)
    (when-let [f (action (codename (.-keyCode e)))]
      (.preventDefault e)
      (swap! world/app-state maybe-step f))))

(defn on-js-reload []
  (println "Reloaded...")
  (reset! world/app-state (world/new-world))
  (reagent/render-component [view/root-view] (. js/document (getElementById "app")))
  
  (when-let [interval-id (-> app-state deref :tick-interval-id)]
    (js/clearInterval interval-id))
  (let [tick-interval (-> app-state deref :tick-interval)]
    (js/console.log "Setting tick-interval to: %s" tick-interval)
    (swap! app-state
           assoc :tick-interval-id (js/setInterval world/tick! tick-interval))))

(defn init []
  (on-js-reload)
  (.addEventListener js/document "keydown" handle-keydown))

(defonce start
  (init))
