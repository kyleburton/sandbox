(ns com.github.kyleburton.clj-xpath-test
  (:use clojure.contrib.test-is)
  (:require
      [com.github.kyleburton.clj-xpath :as xp]))

(deftest test-xp-top-tag
  (is (= :foo
         (xp/$x->tag "/*" (xp/tag :foo "this is a foo")))))

(deftest test-xp-get-body
  (is (= "this is a foo"
         (xp/$x->text "/*" (xp/tag :foo "this is a foo")))))

(deftest test-xp-get-attrs
  (is (= "bobby tables"
         (:name
          (xp/$x->attrs "/*" (xp/tag [:foo :name "bobby tables"]
                                     "drop tables"))))))
