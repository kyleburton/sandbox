(ns personal-portal.web.helpers
  (:require
   [clojure.data.json :as json]))

(defn http-200-json [body]
  {:status  200
   :headers {"Content-type" "application/json"}
   :body    (json/write-str body)})
