(ns logback-riemann-appender.core-test
  (:require
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]))

(deftest test-logging
  (is (= 0 (.get com.github.kyleburton.logback.RiemannAppender/timesCalled)))
  (log/infof "log at info level")
  (is (= 1 (.get com.github.kyleburton.logback.RiemannAppender/timesCalled)))
  (log/fatalf "log at fatal level")
  (is (= 2 (.get com.github.kyleburton.logback.RiemannAppender/timesCalled))))
