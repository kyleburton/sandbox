(ns game-of-life.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::name
 (fn [db]
   (:name db)))

(re-frame/reg-sub
 ::active-panel
 (fn [db _]
   (:active-panel db)))

(re-frame/reg-sub
 ::grid
 (fn [db _]
   (:grid db)))

(re-frame/reg-sub
 ::ui-state
 (fn [db _]
   (:ui-state db)))
