(ns scratchpad.hrm-test
  (:require
   [scratchpad.hrm  :as hrm]
   [clojure.test    :refer :all]
   [scratchpad.core :refer :all]))


(deftest make-new-mem-test
  (testing "creating default new memory"
    (let [mem (hrm/make-new-mem!)]
      (is (= 0 (::hrm/size mem)))
      (is (= {} (::hrm/cells mem)))))
  (testing "creating sized new memory"
    (let [mem (hrm/make-new-mem! {::hrm/size 32})]
      (is (= 32 (::hrm/size mem)))
      (is (= 32 (count (::hrm/cells mem))))))
  (testing "creating new memory with some values"
    (let [mem (hrm/make-new-mem! {::hrm/size 8 ::hrm/vals {0 :banana 7 0}})]
      (is (= :banana
             (hrm/mem-get-val mem 0)))
      (is (= 0 (hrm/mem-get-val mem 7))))))

(deftest get-set-test
  (testing "mem-get and mem-set!"
    (let [mem (hrm/make-new-mem! {::hrm/size 8 ::hrm/vals {0 :banana}})
          mem (hrm/mem-set! mem 0 :apple)]
      (is (= :apple (hrm/mem-get-val mem 0))))))

(deftest get-set-indirect-test
  (testing "mem-set-indirect and mem-get-indirect"
    (let [mem (hrm/make-new-mem! {::hrm/size 8 ::hrm/vals {0 :banana 1 0}})
          mem (hrm/mem-set-indirect! mem 1 :apple)]
      (is (= :apple (hrm/mem-get-val-indirect mem 1))))))


(deftest execute-tests
  (testing "run off end of code"
    (let [[result ctx] (hrm/execute! {::hrm/code [[::hrm/label :start]]})]
      (is (= ::hrm/error result))
      (is (= ::hrm/err-ran-past-end-of-code (-> ctx ::hrm/error first)))))
  (testing "empty inbox to empty outbox"
    (let [[result ctx] (hrm/execute!
                        {::hrm/inbox []
                         ::hrm/mem   (hrm/make-new-mem! {::hrm/size 8})
                         ::hrm/code  [[::hrm/label ::hrm/start]
                                      [::hrm/inbox]
                                      [::hrm/outbox]
                                      [::hrm/jmp ::hrm/start]]})]
      (is (= ::hrm/halted result))
      (is (= [] (::hrm/outbox ctx)))))
  (testing "move all inbox entries to the outbox"
    (let [[result ctx] (hrm/execute!
                        {::hrm/inbox [0 1 2 3]
                         ::hrm/mem   (hrm/make-new-mem! {::hrm/size 8})
                         ::hrm/code  [[::hrm/label ::hrm/start]
                                      [::hrm/inbox]
                                      [::hrm/outbox]
                                      [::hrm/jmp ::hrm/start]]})]
      (is (= ::hrm/halted result))
      (is (= [0 1 2 3] (::hrm/outbox ctx)))))
  (testing "move all positive entries to the outbox"
    (let [[result ctx] (hrm/execute!
                        {::hrm/inbox [0 -1 2 -3 4 -5 6 -7]
                         ::hrm/mem   (hrm/make-new-mem! {::hrm/size 8})
                         ::hrm/code  [[::hrm/label :start]
                                      [::hrm/inbox]
                                      [::hrm/jmpn :skip]
                                      [::hrm/outbox]
                                      [::hrm/label :skip]
                                      [::hrm/jmp :start]]})]
      (is (= ::hrm/halted result))
      (is (= [0 2 4 6] (::hrm/outbox ctx)))))
  (testing "add and subtract"
    (let [[result ctx] (hrm/execute!
                        {::hrm/inbox []
                         ::hrm/mem   (hrm/make-new-mem! {::hrm/size 8 ::hrm/vals {0 10
                                                                                  1 0}})
                         ::hrm/code  [[::hrm/label :start]
                                      [::hrm/copyfrom 1]
                                      [::hrm/sub 0]
                                      [::hrm/jmpz :done]
                                      [::hrm/bump+ 1]
                                      [::hrm/jmp :start]
                                      [::hrm/label :done]
                                      [::hrm/copyfrom 0]
                                      [::hrm/outbox]
                                      [::hrm/copyfrom 1]
                                      [::hrm/outbox]
                                      [::hrm/inbox]]})]
      (is (= ::hrm/halted result))
      (is (= [10 10] (::hrm/outbox ctx))))))



;; https://stackoverflow.com/questions/5570826/how-can-i-undefine-a-function-in-clojure
;; (.unbindRoot #'a-test)
;; (ns-unmap 'scratchpad.hrm-test 'a-test)
;; (deftest a-test
;;   (testing "FIXME, I fail."
;;     (is (= 0 1))))
