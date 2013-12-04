(ns logback-riemann-appender.core-test
  (:require
   [clojure.test :refer :all]
   [clojure.tools.logging :as log]))

(deftest test-logging
  (is (= 0 (.get com.github.kyleburton.logback.RiemannAppender/timesCalled)))

  (log/infof "log at info level")
  (is (= 1 (.get com.github.kyleburton.logback.RiemannAppender/timesCalled)))

  (log/fatalf "log at fatal level")
  (is (= 2 (.get com.github.kyleburton.logback.RiemannAppender/timesCalled)))

  (let [ex (RuntimeException. "Test Error")]
   (log/fatalf ex "log with exception: %s" ex))
  (is (= 3 (.get com.github.kyleburton.logback.RiemannAppender/timesCalled))))


(comment


  (dotimes [ii 20]
    (log/info "test from clojure"))

)