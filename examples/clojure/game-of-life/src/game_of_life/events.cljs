(ns game-of-life.events
  (:require
   [re-frame.core         :as re-frame]
   [game-of-life.db       :as db]
   [day8.re-frame.tracing :refer-macros [fn-traced]]
   [game-of-life.canvas   :as canvas]))

(re-frame/reg-event-db
 ::initialize-db
 (fn-traced
  [_ _]
  (db/generate-default-db)))

(re-frame/reg-event-fx
 ::navigate
 (fn-traced
  [_ [_ handler]]
  {:navigate handler}))

(re-frame/reg-event-fx
 ::set-active-panel
 (fn-traced
  [{:keys [db]} [_ active-panel]]
  {:db (assoc db :active-panel active-panel)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defonce timer (atom nil))

(re-frame/reg-event-fx
 ::gol-start
 (fn-traced
  [{:keys [db]} [_]]
  (when (nil? @timer)
    (let [timer-id (js/setInterval
                    (fn timer-fn []
                      (re-frame/dispatch [::gol-step])) 0)]
      (js/console.log "[INFO|game-of-life.events/::gol-start]: started timer id=%o" timer-id)
      (reset! timer timer-id)))
  {:db (db/start db)}))

(re-frame/reg-event-fx
 ::gol-stop
 (fn-traced
  [{:keys [db]} [_]]
  (when-not (nil? @timer)
    (js/clearTimeout @timer)
    (reset! timer nil))
  {:db (db/stop db)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(re-frame/reg-event-fx
 ::gol-reset
 (fn-traced
  [{:keys [db]} [_]]
  {:db (db/grid db (db/reset-grid (:grid db)))}))

(re-frame/reg-event-fx
 ::gol-new-random
 (fn-traced
  [{:keys [db]} [_]]
  {:db (db/grid db (db/generate-grid (:grid db)))}))

(re-frame/reg-event-fx
 ::gol-update-height
 (fn-traced
  [{:keys [db]} [_ _evt new-val]]
  {:db (db/height db new-val)}))

(re-frame/reg-event-fx
 ::gol-update-width
 (fn-traced
  [{:keys [db]} [_ _evt new-val]]
  {:db (db/width db new-val)}))

;; (re-frame/reg-event-fx
;;  ::gol-gen-next
;;  (fn-traced
;;   [{:keys [db]} [_ _evt new-val]]
;;   {:db (db/grid db (-> db :grid db/next-grid))}))

(re-frame/reg-event-fx
 ::gol-step
 (fn-traced
  [{:keys [db]} [_]]
  (js/setTimeout canvas/render-gol-canvas 0)
  {:db (db/grid db (-> db :grid db/next-grid))}))

(re-frame/reg-event-fx
 ::gol-go-back
 (fn-traced
  [{:keys [db]} [_]]
  {:db (db/go-back db)}))

(re-frame/reg-event-fx
 ::gol-resize-matrix
 (fn-traced
  [{:keys [db]} [_]]
  {:db (db/resize-matrix! db)}))

(re-frame/reg-event-fx
 ::toggle-cell
 (fn-traced
  [{:keys [db]} [_ yy xx]]
  {:db (db/toggle-cell db yy xx)}))


(re-frame/reg-event-fx
 ::document-keyup
 (fn-traced
  [{:keys [db]} [_ evt]]
  (cond
    (or (= "s" (.-key evt)) (= "S" (.-key evt)))
    (do
      (re-frame/dispatch [::gol-step])
      (js/setTimeout canvas/render-gol-canvas 0))

    :else
    (js/console.log "[INFO|game-of-life.events/$fn]: unrecognized key: %o" (.-key evt)))
  {:db db}))

(re-frame/reg-event-fx
 ::place-object
 (fn-traced
  [{:keys [db]} [_ object]]
  (js/console.log "[INFO|game-of-life.events/::place-object]: object=%o" object)
  (case object
    :glider :ok
    :hwss   :ok)
  {:db db}))
