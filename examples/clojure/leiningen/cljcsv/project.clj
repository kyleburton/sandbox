(defproject cljcsv "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :dependencies 
    [[org.clojure/clojure "1.1.0"]
     [org.clojure/clojure-contrib "1.1.0"]
     [clojure-csv/clojure-csv "1.0.0"]
     ; [csvclj "1.0.0"]
  ]
  :dev-dependencies
    [[swank-clojure "1.2.1"]]
  :aot  [cljcsv.core]
  ;; :main [cljcsv.core]
  :warn-on-reflection true
  :main cljcsv.core
)
