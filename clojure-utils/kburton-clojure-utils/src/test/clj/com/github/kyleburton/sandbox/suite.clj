(ns com.github.kyleburton.sandbox.suite
  (:use [clojure.contrib.test-is :as test-is])
  (:require [com.github.kyleburton.sandbox.rabbitmq-test]
            [com.github.kyleburton.sandbox.utils-test]))

(run-tests 'com.github.kyleburton.sandbox.rabbitmq-test
           'com.github.kyleburton.sandbox.utils-test)

