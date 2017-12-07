(ns text-adventure-engine.app
  (:require [text-adventure-engine.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
