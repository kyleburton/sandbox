(ns com.github.kyleburton.sandbox.regex
  (:import (java.util.regex Pattern Matcher))
  (:use [com.github.kyleburton.sandbox.utils :as kutils]
        [com.github.kyleburton.sandbox.ref-data :as ref-data])
  (:use [clojure.contrib.str-utils :as str]
        [clojure.contrib.fcase :only (case)]))


;; regexes, initial set pulled from Regex::Common CPAN module
(def *common-regexes* 
  {:num-real #"(?-xism:(?:(?i)(?:[+-]?)(?:(?=[0123456789]|[.])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)(?:(?:[E])(?:(?:[+-]?)(?:[0123456789]+))|)))"
   :num-int #"(?-xism:(?:(?:[+-]?)(?:[0123456789]+)))"
   :num-decimal #"(?-xism:(?:(?i)(?:[+-]?)(?:(?=[0123456789]|[.])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)))"
   :num-hex #"(?-xism:(?:(?i)(?:[+-]?)(?:(?=[0123456789ABCDEF]|[.])(?:[0123456789ABCDEF]*)(?:(?:[.])(?:[0123456789ABCDEF]{0,}))?)(?:(?:[G])(?:(?:[+-]?)(?:[0123456789ABCDEF]+))|)))"
   :num-dec #"(?-xism:(?:(?i)(?:[+-]?)(?:(?=[0123456789]|[.])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)(?:(?:[E])(?:(?:[+-]?)(?:[0123456789]+))|)))"
   :num-oct #"(?-xism:(?:(?i)(?:[+-]?)(?:(?=[01234567]|[.])(?:[01234567]*)(?:(?:[.])(?:[01234567]{0,}))?)(?:(?:[E])(?:(?:[+-]?)(?:[01234567]+))|)))"
   :num-bin #"(?-xism:(?:(?i)(?:[+-]?)(?:(?=[01]|[.])(?:[01]*)(?:(?:[.])(?:[01]{0,}))?)(?:(?:[E])(?:(?:[+-]?)(?:[01]+))|)))"
   :num-roman #"(?-xism:(?xi)(?=[MDCLXVI])
                         (?:M{0,3}
                            (D?C{0,3}|CD|CM)?
                            (L?X{0,3}|XL|XC)?
                            (V?I{0,3}|IV|IX)?))"
   :zip #"(?-xism:(?:(?:(?:USA?)-){0,1}(?:(?:(?:[0-9]{3})(?:[0-9]{2}))(?:(?:-)(?:(?:[0-9]{2})(?:[0-9]{2}))){0,1})))"
   :net-ipv4 #"(?-xism:(?:(?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})[.](?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})[.](?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})[.](?:25[0-5]|2[0-4][0-9]|[0-1]?[0-9]{1,2})))"
   :net-mac #"(?-xism:(?:(?:[0-9a-fA-F]{1,2}):(?:[0-9a-fA-F]{1,2}):(?:[0-9a-fA-F]{1,2}):(?:[0-9a-fA-F]{1,2}):(?:[0-9a-fA-F]{1,2}):(?:[0-9a-fA-F]{1,2})))"
   :net-domain #"(?-xism:(?: |(?:[A-Za-z](?:(?:[-A-Za-z0-9]){0,61}[A-Za-z0-9])?(?:\.[A-Za-z](?:(?:[-A-Za-z0-9]){0,61}[A-Za-z0-9])?)*)))"
   :phone #"(?:1[- ]?)?\(?[2-9]\d{2}\)?[-\. ]?\d{3}[-\. ]?\d{4}(?:\s*(?:e|ex|ext|x|xtn|extension)?\s*\d*)"
   :us-states        (Pattern/compile (format "(?-xism:%s)" (str/str-join "|" (keys ref-data/*us-states*))))
   :us-state-names   (Pattern/compile (format "(?-xism:%s)" (str/str-join "|" (vals ref-data/*us-states*))))
   :us-airport-codes (Pattern/compile (format "(?-xism:%s)" (str/str-join "|" (map #(nth % 2) ref-data/*us-airport-codes*))))
   :us-area-codes    (Pattern/compile (format "(?-xism:%s)" (str/str-join "|" ref-data/*us-area-codes*)))


   :word           #"(?:[\w-]+)"
   :punctuation    #"(?:[\.,\?/'\";:\\`~!\(\)]+)"
   })

;; (re-find (:us-airport-codes *common-regexes*) "foo PHL bar")
;; (re-find (:us-area-codes *common-regexes*) "foo 484 bar")
;; (re-find (:word *common-regexes*) "foo 484 bar")
;; (re-find (:punctuation *common-regexes*) "foo 484 , bar")


;; (:us-state-names *common-regexes*)
;; (:us-states *common-regexes*)
;; (re-matches (:zip *common-regexes*) "19087")



;; (re-matches (:phone *common-regexes*) "1 (610) 940 4002 x 116")
;; (re-matches (:phone *common-regexes*) "1 (610) 940 4002x116")
;; (re-matches (:phone *common-regexes*) "(610) 940 4002 x 116")
;; (re-matches (:phone *common-regexes*) "610.940.4002")
;; (re-matches (:phone *common-regexes*) "610.940.4002")
;; (re-matches (:phone *common-regexes*) "1610.940.4002")
;; (re-matches (:phone *common-regexes*) "1-610.940.4002")
;; (re-matches (:phone *common-regexes*) "1 610.940.4002")

