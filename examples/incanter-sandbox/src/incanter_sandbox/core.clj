(ns incanter-sandbox.core
  (:require [incanter.core :as incanter]
            [incanter.stats :as stats]
            [incanter.charts :as charts]))


(comment

(view (histogram (repeatedly 150 #(rand-int 10))))

(incanter/view
 (charts/histogram
  (repeatedly 150 #(rand-int 10))))

(incanter/view (charts/histogram (stats/sample-normal 1000)))

(defn sin [x]
  (Math/sin x))

(incanter/view (charts/function-plot sin -10 10))

)
