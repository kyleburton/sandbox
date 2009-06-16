;; TODO: support authorization
(ns com.github.kyleburton.sandbox.web
  (:import (org.apache.commons.httpclient          HttpClient NameValuePair)
           (org.apache.commons.httpclient.methods  GetMethod PostMethod)
           (org.apache.commons.httpclient.auth     AuthScope)
           (org.apache.commons.httpclient          UsernamePasswordCredentials))
  (:require [com.github.kyleburton.sandbox.landmark-parser :as lparse]
            [com.github.kyleburton.sandbox.utils :as kutils]
            [com.github.kyleburton.sandbox.swing :as sw])
  (:use [clojure.contrib.str-utils :as str]
        [clojure.contrib.fcase :only (case)]))

(def *ua* (org.apache.commons.httpclient.HttpClient.))

(defn map->nvpairs [m]
  (if-let [names (keys m)]
    (into-array (vec (map #(NameValuePair. % (m %)) (keys m))))
    nil))

(defn ua-get [ua url & [params]]
  (let [req (GetMethod. url)
        pairs (map->nvpairs params)]
    (if pairs
      (.setQueryString req pairs))
    (.setFollowRedirects req true)
    (.executeMethod ua req)
    req))

(defn ua-get->string [ua url & [params]]
  (.getResponseBodyAsString (apply ua-get ua url params)))

(defn #^String get->string [#^String url & [params]]
  (apply ua-get->string *ua* url params))

;; (get->string "http://google.com/")
;; (get->string "http://intranet.hmsonline.com/confluence/display/SWDEV/Home")

(defn ua-post [ua url params]
  (let [req (PostMethod. url)
        pairs (map->nvpairs params)]
    (if pairs
      (.setRequestBody req pairs))
    (.executeMethod ua req)
    req))

(defn ua-post->string [ua url params]
  (.getResponseBodyAsString (ua-post ua url params)))

(defn post->string [url params]
  (ua-post->string *ua* url params))

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
     (if lg
       (recur (.replceAll html lg (*ligature->chr* lg))
              lgs)
       html))
   "&amp;" "&"))

(defmacro with-http-client [[client & params] & body]
  `(let [params#   (second (kutils/parse-paired-arglist [~@params]))
         client#   (org.apache.commons.httpclient.HttpClient.)
         ~client   client#]
     (when (:user params#)
       (.setAuthenticationPreemptive (.getParams client#) true)
       (.setCredentials (.getState client#)
                        (AuthScope. (:host params#)
                                    (:port params#)
                                    AuthScope/ANY_REALM)
                        (UsernamePasswordCredentials. (:user params#)
                                                      (:pass params#))))
     ~@body))

(defn form-target [base-uri form]
  (let [action (:action form)
        base (re-sub #"/[^/]*$" "/" (str base-uri))]
    ;; if the action already has a schema on it, just return it
    (cond (re-find #"^http" action)
          action
          true
          (str base action))))
