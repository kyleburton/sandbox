;; http://www.theserverside.com/tt/articles/article.tss?l=UsingCI-Bayes
;; mvn -Dmaven.test.skip=true dependency:copy-dependencies -DoutputDirectory=deps/
;; then copied dep/*.jar to $HOME/.clojure, removing duplicate libs

(ns com.github.kyleburton.sandbox.cl-bayes
  (:import (com.enigmastation.classifier.impl NaiveClassifierImpl))
  (:require [com.github.kyleburton.sandbox.utils :as kutils]
            [com.github.kyleburton.sandbox.web :as web]
            [clojure.contrib.duck-streams :as ds]
            [clojure.contrib.str-utils :as str]))

(def classifier (NaiveClassifierImpl.))

(def non-prof-urls
     ["http://www.uhmchealth.com/"
      "http://www.uhmchealth.com/PhysRef.asp"
      "http://www.uhmchealth.com/PhysRefResults.asp?strFirstName=a"])

(def prof-urls
     ["http://www.uhmchealth.com/PhysRefProfile.asp?guidPhysicianID=%7B154DDEB8-FE46-4B5F-A3C5-4A289B806E1F%7D"
      "http://www.uhmchealth.com/PhysRefProfile.asp?guidPhysicianID=%7B4010E922-FCBA-40DC-B394-80E675C75684%7D"
      "http://www.uhmchealth.com/PhysRefProfile.asp?guidPhysicianID=%7B54220120-DD11-412A-9F6D-32B786E1E659%7D"
      "http://www.uhmchealth.com/PhysRefProfile.asp?guidPhysicianID=%7BAB6DE9A5-54DE-4AE6-A5D7-00B7FC832767%7D"])

(do
  (dorun
   (doseq [url non-prof-urls]
     (.train classifier (web/memoized-get->string url) "no-profile")))
  (prn "trained"))

(do
  (dorun
   (doseq [url prof-urls]
     (.train classifier (web/memoized-get->string url) "has-profile")))
  (prn "trained"))


(.getClassification classifier (web/memoized-get->string "http://www.uhmchealth.com/CCForm.asp"))
;; "no-profile"
(.getClassification classifier (web/memoized-get->string "http://www.uhmchealth.com/PhysRefProfile.asp?guidPhysicianID=%7B1E26B0A4-36DE-46C4-A041-3557407C1A34%7D"))
;; "has-profile"

(.getClassification classifier (web/memoized-get->string "http://google.com"))
;; "no-profile"


(.getClassification classifier (web/memoized-get->string "http://www.uhhospitals.org/"))
;; "no-profile"
(.getClassification classifier (web/memoized-get->string "http://www.uhhospitals.org/PhysicianFinder/tabid/1473/Default.aspx"))
;; "no-profile"
(.getClassification classifier (web/memoized-get->string "http://www.uhhospitals.org/PhysicianFinder/tabid/1473/sIsSearch/1/Default.aspx"))
;; "no-profile"

;; our first false positive:
(.getClassification classifier (web/memoized-get->string "http://www.uhhospitals.org/PhysicianFinder/PhysicianDetails/tabid/1709/phyID/9300/Default.aspx"))
;; "no-profile"

;; so lets train it some more:
(.train classifier (web/memoized-get->string "http://www.uhhospitals.org/PhysicianFinder/PhysicianDetails/tabid/1709/phyID/9300/Default.aspx") "has-profile")
(.train classifier (web/memoized-get->string "http://www.uhhospitals.org/PhysicianFinder/PhysicianDetails/tabid/1709/phyID/3518/Default.aspx") "has-profile")

;; now how does it do?
(.getClassification classifier (web/memoized-get->string "http://www.uhhospitals.org/PhysicianFinder/PhysicianDetails/tabid/1709/phyID/9300/Default.aspx"))
;; "has-profile"

;; good, what about the old profiles?

;; good, what about on new unseen?
;; 
(.getClassification classifier (web/memoized-get->string "http://www.uhhospitals.org/PhysicianFinder/PhysicianDetails/tabid/1709/phyID/3290/Default.aspx"))
;; "has-profile"
(.getClassification classifier (web/memoized-get->string "http://www.uhhospitals.org/PhysicianFinder/PhysicianDetails/tabid/1709/phyID/3293/Default.aspx"))
;; "has-profile"