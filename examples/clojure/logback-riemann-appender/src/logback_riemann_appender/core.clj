(ns logback-riemann-appender.core
  (:require
   [clojure.tools.logging :as log]))

(defn log-something []
  (log/infof "log at info level")
  (log/fatalf "log at fatal level"))



(comment

  (log-something)

)