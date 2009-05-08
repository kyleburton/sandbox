;; TODO: support authorization
(ns com.github.kyleburton.sandbox.web
  (:import (org.apache.commons.httpclient HttpClient NameValuePair)
           (org.apache.commons.httpclient.methods GetMethod))
  (:require [com.github.kyleburton.sandbox.landmark-parser :as lparse]
            [com.github.kyleburton.sandbox.utils :as kutils])
  (:use [clojure.contrib.str-utils :as str]
        [clojure.contrib.fcase :only (case)]))


(def *ua* (org.apache.commons.httpclient.HttpClient.))

(defn map->nvpairs [m]
  (if-let [names (keys m)]
    (into-array (vec (map #(NameValuePair. (.getName %) (m %)) (keys m))))
    nil))

(defn #^String get->string [#^String url & [params]]
  (let [req (GetMethod. url)
        pairs (map->nvpairs params)]
    (if pairs
      (.setQueryString req pairs))
    (.executeMethod *ua* req)
    (.getResponseBodyAsString req)))

(def memoized-get->string
     (fn [& params]
       (apply get->string params)))

(defn strip-html [#^String html]
  (.replaceAll html "<[^>]+>" ""))

(def *ligature->chr* 
     {"&gt;"   ">"
      "&lt;"   "<"
      "&nbsp;" " "
      })

(defn html-decode [#^String html]
  (.replaceAll 
   (loop [html html
          [lg & lgs] (keys *ligature->chr*)]
     (prn (format "html-decode: html=%s lg=%s lgs=%s" html lg lgs))
     (if lg
       (recur (.replceAll html lg (*ligature->chr* lg))
              lgs)
       html))
   "&amp;" "&"))

