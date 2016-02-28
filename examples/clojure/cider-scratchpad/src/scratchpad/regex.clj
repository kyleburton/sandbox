(ns scratchpad.regex
  (:import
   [java.util.regex Pattern]))

;; http://stackoverflow.com/questions/34001939/regular-expression-for-norwegian-numbers

(def test-cases
  [{:expectation true
    :number      "90909090"
    :note        "normal number"}

   {:expectation true
    :number      "4790909090"
    :note        "number with country code"}
   
   {:expectation true
    :number      "+4790909090"
    :note        "country code using +"}
   
   {:expectation true
    :number      "004790909090"
    :note        ", country code using 00"}

   {:expectation false
    :number "+47909090"
    :note ", without country code or too short number"}
   
   {:expectation false
    :number "9090909o"
    :note ",  invalid character"}
   
   {:expectation false
    :number "9090909"
    :note ",  too few digits"}
   
   {:expectation false
    :number "+4690909090"
    :note ", wrong country code"}
   
   {:expectation false
    :number "909090909"
    :note ", too many digits"}
   
   {:expectation false
    :number "00474790909090"
    :note " Trying to fool the regex now"}
   
   ])
