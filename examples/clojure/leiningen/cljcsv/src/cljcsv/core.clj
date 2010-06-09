(ns cljcsv.core
  (:require [com.davidsantiago.csv :as cdc]
            ;[com.github.jonase.csv :as cgjc]
            [clojure.contrib.duck-streams :as ds]
            [clojure.contrib.pprint :as pp])
  (:gen-class))

(defn parse-cdc [file]
 (let [rs (cdc/parse-csv (slurp file))]
  (prn (str "rs=" rs))
  (prn (pp/cl-format nil "cdc: rows: ~a~&" (count rs)))
  (prn (pp/cl-format nil "cdc: ~a~&" rs))))

;; Requires 1.2-SNAPSHOT, can't test for now
;(defn parse-cgjc [file]
; (let [rs (cgjc/parse (ds/reader file))]
;  (prn (str "rs=" rs))
;  (prn (pp/cl-format nil "cgjc: rows: ~a~&" (count rs)))
;  (prn (pp/cl-format nil "cgjc: ~a~&" rs))))

(defn -main [& args]
  (prn (format "args=%s" args))
  (if (not (empty? args))
    (do
      (parse-cdc (first args))
      ;;(parse-cgjc (first args))
      )))

