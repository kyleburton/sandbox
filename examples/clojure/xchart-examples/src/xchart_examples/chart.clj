(ns xchart-examples.chart
  (:require
   [com.hypirion.clj-xchart :as chart]))

;; https://github.com/hypirion/clj-xchart



(comment
  ;; two line x y chart
  (let [r (java.util.Random.)  #_(java.util.Random. 42)]
    (chart/view
     (chart/xy-chart
      {"Maxime" {:x (range 10)
                 :y (mapv #(+ % (* 3 (.nextDouble r)))
                          (range 10))}
       "Tyrone" {:x (range 10)
                 :y (mapv #(+ 2 % (* 4 (.nextDouble r)))
                          (range 0 5 0.5))}}
      {:title "Longest running distance"
       :x-axis {:title "Months (since start)"}
       :y-axis {:title "Distance"
                :decimal-pattern "##.## km"}
       :theme :matlab})))


  (let [r      (java.util.Random.)  ;; (java.util.Random. 42)
        ygenfn (fn [base]
                 (mapv #(+ % (* base (.nextDouble r)))
                       (range 10)))]
    (chart/view
     (chart/xy-chart
      {"Set A" {:x (range 10) :y (ygenfn 3)}
       "Set B" {:x (range 10) :y (ygenfn 4)}
       "Set C" {:x (range 10) :y (ygenfn 5)}}
      {:title  "Longest running distance"
       :x-axis {:title "Months (since start)"}
       :y-axis {:title "Distance"
                :decimal-pattern "##.## km"}
       :theme  :matlab})))


  )



(comment

  (chart/view
   (chart/xy-chart
    {"Error bars" {:x          (range 0 100 10)
                   :y          [20 30 45 40 60 65 80 75 95 90]
                   :error-bars [5 8 2 9 3 3 8 3 9 3]}}
    {:error-bars-color :match-series}))

  (chart/view
   (chart/xy-chart
    {"Error bars" {:x          (range 0 10)
                   :y          [2.0 3.0 4.5 4.0 6.0 6.5 8.0 7.5 9.5 9.0]       ;; (mapv #(/ % 10.0) [20 30 45 40 60 65 80 75 95 90])
                   :error-bars [5 8 2 9 3 3 8 3 9 3]}}
    {:error-bars-color :match-series}))
  )


(comment


  ;; area charts

  (chart/view
   (chart/xy-chart
    {"Memory usage" {:x (range 0 10 0.5)
                     :y [0.0 0.5 2.3 4.5 2.7 4.5 6.7
                         9.0 9.3 9.5 6.7 7.5 8.8 10.3
                         9.7 11.4 5.6 4.5 5.6 1.2]
                     :style {:marker-type :none}}
     "Total memory" {:x [-100 100]
                     :y [12 12]
                     :style {:render-style :line
                             :marker-type :none
                             :line-color :red}}}
    {:title "Memory usage"
     :render-style :area
     :x-axis {:title "Time (min)"
              :min 0
              :max 10}
     :y-axis {:title "Memory (GB)"
              :max 15}
     :legend {:position :inside-nw}}))


  ;; (.nextBoolean (java.util.Random.))
  (let [r (java.util.Random.)
        jitter (fn [x]
                 (if (.nextBoolean r)
                   (+ x (* (.nextFloat r) x -1))
                   (+ x (* (.nextFloat r) x))))]
    (chart/view
     (chart/xy-chart
      {"Memory usage" {:x (range 0 10 0.5)
                       :y [0.0 0.5 2.3 4.5 2.7 4.5 6.7
                           9.0 9.3 9.5 6.7 7.5 8.8 10.3
                           9.7 11.4 5.6 4.5 5.6 1.2]
                       :style {:marker-type :none}}
       "Heap Usage"   {:x (range 0 10 0.5)
                       :y (mapv #(jitter (* % 0.90))
                                [0.0 0.5 2.3 4.5 2.7 4.5 6.7
                                 9.0 9.3 9.5 6.7 7.5 8.8 10.3
                                 9.7 11.4 5.6 4.5 5.6 1.2])
                       :style {:marker-type :none}}
       "Total memory" {:x [-100 100]
                       :y [12 12]
                       :style {:render-style :line
                               :marker-type :none
                               :line-color :red}}}
      {:title "Memory usage"
       :render-style :area
       :x-axis {:title "Time (min)"
                :min 0
                :max 10}
       :y-axis {:title "Memory (GB)"
                :max 15}
       :legend {:position :inside-nw}})))

  )



(comment

  ;; bar charts

  (chart/view
   (chart/category-chart
    {"Bananas" {"Mon" 6, "Tue" 2, "Fri" 3, "Wed" 1, "Thur" 3}
     "Apples"  {"Tue" 3, "Wed" 5, "Fri" 1, "Mon" 1}
     "Pears"   {"Thur" 1, "Mon" 3, "Fri" 4, "Wed" 1}}
    {:title "Weekly Fruit Sales"
     :theme :ggplot2
     :x-axis {:order ["Mon" "Tue" "Wed" "Thur" "Fri"]}}))


  (let [r          (java.util.Random.)
        days       ["Mon" "Tue" "Fri" "Wed" "Thur"]
        sales-data (reduce
                    (fn [acc day]
                      (assoc acc day (-> (.nextInt (java.util.Random.) 10) (+ 2))))
                    {}
                    days)]
    (chart/view
     (chart/category-chart
      ;; {"Sales" {"Mon" 6, "Tue" 2, "Fri" 3, "Wed" 1, "Thur" 3}}
      {"Sales" sales-data}
      {:title "Weekly Fruit Sales"
       :theme :ggplot2
       :x-axis {:order days}})))
  )

(comment

  ;; line / stick charts

  (chart/view
   (chart/category-chart
    (chart/transpose-map
     {"Easy to find?" {"True"      1329,
                       "False"     47,
                       "Mixed bag" 830},
      "Active maintainers?" {"True"      1049,
                             "False"     32,
                             "Mixed bag" 1015},
      "Accurate + good docs?" {"True"      435,
                               "False"     295,
                               "Mixed bag" 1463},
      "Good quality?" {"True"      1221,
                       "False"     36,
                       "Mixed bag" 910}})
    {:title        "Excerpt from the State of Clojure Survey 2015"
     :render-style :stick
     :y-axis       {:ticks-visible? false}
     :x-axis       {:label {:rotation 30}}}))
  
  )


