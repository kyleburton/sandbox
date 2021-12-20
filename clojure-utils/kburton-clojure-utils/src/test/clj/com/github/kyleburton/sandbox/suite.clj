(ns com.github.kyleburton.sandbox.suite
  (:require [com.github.kyleburton.sandbox.rabbitmq-test]
            [com.github.kyleburton.sandbox.utils-test]))

(comment

  (run-tests 'com.github.kyleburton.sandbox.rabbitmq-test
             'com.github.kyleburton.sandbox.utils-test)

)

