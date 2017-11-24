(ns the-mystery-on-orville-st.actions-test
  (:require [clojure.test :refer :all]
            [the-mystery-on-orville-st.actions :refer :all]))

(def fixtures
  {:actions {:test-action (map->Action
                           {:name        :test-action
                            :description ["Test the action."]
                            :aliases     #{"test" "test the action"}
                            :action-fn   (fn [game-state player]
                                           :ok)})}})

(deftest test-describe-action
  (testing "Actions"
    (testing "can be described"
      (is (=
           "Test the action. (Try one of 'test the action'; 'test')"
           (describe-action (-> fixtures :actions :test-action)))))))


(comment

  (run-tests)

)
