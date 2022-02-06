(ns game-of-life.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [game-of-life.core :as core]))

(deftest fake-test
  (testing "fake description"
    (is (= 1 2))))
