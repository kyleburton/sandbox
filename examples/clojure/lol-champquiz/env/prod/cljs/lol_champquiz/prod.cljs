(ns lol-champquiz.prod
  (:require [lol-champquiz.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
