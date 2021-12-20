(ns mysterious_messages.test-runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   [mysterious_messages.core-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
       (run-tests
        'mysterious_messages.core-test))
    0
    1))
