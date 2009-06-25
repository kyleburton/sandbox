(ns com.github.kyleburton.app
  (:gen-class)
  (:use [com.github.kyleburton.sandbox.web :as kweb]))

(defn -main [url]
  (println (format "%s/main: url=%s" *ns* url)))

;compile