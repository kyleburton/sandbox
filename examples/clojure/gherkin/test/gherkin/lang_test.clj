(ns gherkin.lang-test
  (:require [clojure.test :refer :all]
            [gherkin.lang :refer :all]))

(def gherkin-examples
  {:weekly-wages "
Feature: Track time and compute wages in the accounting system.
  As a worker I want to be paid weekly.

  Scenario: System computes weekly wages.
    Given an employee named Bob making $12 per hour.
    When Bob works 40 hours in one week;
    Then Bob will be paid $480 on Friday evening.
"

   :with-table "
Feature: This is a test that inclues a table as an argument.

  Scenario: Scenarios support table data.
  Given a table of data:
    | username     | followers |
    | kburton      | 2         |
    | jsmith       | 999       |
    | bwayne       | 123       |
    | ckent        | 4192      |
  Then the total follower count is 5316
"})


(deftest test-handler-matches
  (let [try-match (fn [m l]
                    (let [[res m2 args] (handler-matches? {:matcher m} l)]
                      [res args]))]
    (is (= [true  []] (try-match "This is a match" "Given This is a match")))
    (is (= [true  []] (try-match "This is a match" "When This is a match")))
    (is (= [true  []] (try-match "This is a match" "Then This is a match")))
    (is (= [true  []] (try-match "This is a match" "And This is a match")))
    (is (= [false []] (try-match "This is _not_ a match" "This is a match")))
    (is (= [true  []] (try-match #"This is a match" "This is a match")))
    (is (= [true  ["Bob" "8"]]
           (try-match #"(\S+) works an (\S+) hour shift" "Bob works an 8 hour shift")))))

(deftest is-feature-line
  (is (is-feature-line? "Feature: some feature" :none))
  (is (not (is-feature-line? "" :none)))
  (is (not (is-feature-line? "" :feature)))
  (is (not (is-feature-line? "" :scenario)))
  (is (not (is-feature-line? "Then I do something" :scenario)))
  (is (not (is-feature-line? "Scenario: some feature" :none))))

(deftest is-scenario-line
  (is (is-scenario-line? "Scenario: some feature" :none))
  (is (is-scenario-line? "    Given an employee named Bob making $12 per hour." :scenario))
  (is (not (is-scenario-line? "" :none)))
  (is (not (is-scenario-line? "" :feature)))
  (is (not (is-scenario-line? "" :scenario)))
  (is (not (is-scenario-line? "Then I do something" :feature)))
  (is (not (is-scenario-line? "Feature: some feature" :none))))

(deftest test-parse-table-line
  (is (= ["foo" "bar"] (parse-table-line {:line "|foo|bar|"})))
  (is (= ["foo" "bar"] (parse-table-line {:line "| foo | bar |"})))
  (is (= ["foo" "bar"] (parse-table-line {:line "| foo |bar |"})))
  (is (= ["foo" "bar"] (parse-table-line {:line "| foo| bar|"}))))

(deftest test-parse-table
  (is (=
       (table-lines->table
        [{:line "| user    | num followers |"}
         {:line "| kburton | 2             |"}
         {:line "| jsmith  | 999           |"}])
       [{:num-followers "2", :user "kburton"}
        {:num-followers "999", :user "jsmith"}])))

(deftest test-run-weekly-wages
  (do
    (reset-registry!)

    (Given #"an employee named (\S+) making \$(\S+) per hour." [ctx name hourly-rate]
           [true
            (update-in ctx
                       [:employees]
                       assoc
                       name
                       {:name        name
                        :hourly-rate (Integer/parseInt hourly-rate)})])
    
    (When #"(\S+) works (\S+) hours in one week;" [ctx name hours-worked]
          [true
           (update-in ctx
                      [:employees name]
                      assoc
                      :hours-worked (Integer/parseInt hours-worked))])
    
    (Then #"(\S+) will be paid \$(\S+) on (\S+) evening." [ctx name total-wages day-of-week]
          (let [employee       (-> ctx :employees (get name))
                hourly-rate    (-> employee :hourly-rate)
                hours-worked   (-> employee :hours-worked)
                computed-wages (* hours-worked hourly-rate)
                total-wages    (Integer/parseInt total-wages)]
            [(and
              (= total-wages computed-wages)
              (= "Friday" day-of-week))
             (update-in
              ctx
              [:employees name]
              :computed-wages computed-wages)]))
    
    (let [result (run-gherkin {} (parse (gherkin-examples :weekly-wages)))]
      (def rr result)
      (:scenario-results result))))

;; (test-run-weekly-wages)

(deftest test-tabular-data
  (reset-registry!)
  (Given "a table of data:" [ctx followers-table]
         (assoc ctx :followers-table followers-table))
  (Then #"the total follower count is (\\S+)" [ctx expected-count]
        (let [actual-count (reduce
                            +
                            (map :followers (:followers-table ctx)))]
          [(= actual-count (Integer/parseInt expected-count))
           ctx]))
  (run-gherkin {} (parse (gherkin-examples :with-table))))

(comment
  (partition-by
   :type
   (->
    (gherkin-examples :weekly-wages)
    parse
    :parsed
    vec))

  (test-run-weekly-wages)
  (run-tests)
  )
