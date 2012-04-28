(ns jsoup.core
  (:require [clj-etl-utils.http :as ua])
  (:import
   [org.jsoup Jsoup]
   [org.jsoup.safety Whitelist])
  (:use
   [clj-etl-utils.lang-utils :only [raise]]))




(def *relay-page* (ua/do-get (ua/user-agent)
                             "http://relaynetwork.com/"))

(defn relay-page []
  (:response-body *relay-page*))

(def *white-list* (Whitelist/relaxed))

(defn strip [html & [white-list]]
  (Jsoup/clean html (or white-list *white-list*)))

(comment


  (spit "foo.html" (strip (relay-page)))

)

