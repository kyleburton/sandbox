(ns com.github.kyleburton.sandbox.web
  (:import (org.apache.commons.httpclient HttpClient NameValuePair))
  (:import (org.apache.commons.httpclient.methods GetMethod))
  (:require [com.github.kyleburton.sandbox.landmark-parser :as lparse]
            [com.github.kyleburton.sandbox.utils :as kutils])
  (:use [clojure.contrib.str-utils :as str]
        [clojure.contrib.fcase :only (case)]))


(def *ua* (org.apache.commons.httpclient.HttpClient.))

(defn #^String get->string [#^String url & [params]]
  (let [req (GetMethod. url)
        pairs (map->nvpairs params)]
    (prn "setQueryString: " pairs)
    (if pairs
      (.setQueryString req pairs))
    (.executeMethod *ua* req)
    (.getResponseBodyAsString req)))

(defn map->nvpairs [m]
  (if-let [names (keys m)]
    (into-array (vec (map #(NameValuePair. (.getName %) (m %)) (keys m))))
    nil))
