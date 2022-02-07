(ns game-of-life.core
  (:require
   [reagent.dom         :as rdom]
   [re-frame.core       :as re-frame]
   [game-of-life.events :as events]
   [game-of-life.routes :as routes]
   [game-of-life.views  :as views]
   [game-of-life.config :as config]))


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/main-panel] root-el))
  (let [keyhandler-el (.getElementById js/document "gol-keyhandler")]
    (rdom/unmount-component-at-node keyhandler-el)
    (rdom/render [views/gol-keyhandler] keyhandler-el)))

(defn init []
  (routes/start!)
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))
