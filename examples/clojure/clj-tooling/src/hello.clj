(ns hello
  (:require
    [clj-time.core :as t]
    [clj-time.format :as f]))

(defn time-str [dt]
  (f/unparse (f/with-zone (f/formatter "hh:mm aa") (t/default-time-zone))
             dt))

(defn -main []
  (println "Hello world, the time is: " (time-str (t/now))))
