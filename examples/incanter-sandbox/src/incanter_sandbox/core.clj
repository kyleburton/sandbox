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

(incanter/view (charts/function-plot
                #(* 10.0 (Math/log %))
                0.0 20.0))

(incanter/view (charts/function-plot
                #(Math/log (* % %))
                1.0 200.0))

;; (incanter/view (charts/line-chart (range 0 20)
;;                                   (map #(* 10.0 (Math/log %)) (range 1.0 20.0))))

;; Example 'penalty' for old data
;;  where x could be weeks in age
(incanter/view (charts/function-plot (fn [x]
                                       (if (<= x 0)
                                         0
                                         (- 10 (+ 1 (/ 10 (inc x))))))
                                     0 20))



)
