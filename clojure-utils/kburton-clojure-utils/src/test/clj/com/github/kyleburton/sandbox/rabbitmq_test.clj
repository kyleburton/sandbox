(ns com.github.kyleburton.sandbox.rabbitmq-test
  (:use [com.github.kyleburton.sandbox.rabbitmq :as rabbitmq]
        [clojure.test :as test]))


(deftest test-make-connection-params
  (let [params (rabbitmq/make-connection-params {})]
    (is (= "guest" (.getUserName params)))
    (is (= "guest" (.getPassword params))))
  (let [params (rabbitmq/make-connection-params {:user "foo" :pass "bar"})]
    (is (= "foo" (.getUserName params)))
    (is (= "bar" (.getPassword params)))))
  

