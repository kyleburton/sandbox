(ns text-adventure-engine.core-test
  (:require [cljs.test :refer-macros [is are deftest testing use-fixtures]]
            [pjstadig.humane-test-output]
            [text-adventure-engine.core :as rc]))

(deftest test-home
  (is (= true true)))

