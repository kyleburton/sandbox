(ns com.github.kyleburton.contrib-examples.base64
  (:require [clojure.contrib.base64 :as base64])
  (:gen-class))


(defn main [& args]
  (doseq [s (map str args)]
    (prn (format "%s => %s\n" s (base64/encode-str s)))))

(comment

  (com.github.kyleburton.contrib-examples.base64/main ["this"])
  )