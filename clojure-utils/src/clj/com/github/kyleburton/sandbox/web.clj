;; TODO: support authorization
(ns com.github.kyleburton.sandbox.web
  (:import (org.apache.commons.httpclient          HttpClient NameValuePair)
           (org.apache.commons.httpclient.methods  GetMethod)
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
    (into-array (vec (map #(NameValuePair. (.getName %) (m %)) (keys m))))
    nil))

(defn ua-get [ua url & [params]]
  (let [req (GetMethod. url)
        pairs (map->nvpairs params)]
    (if pairs
      (.setQueryString req pairs))
    (.setFollowRedirects req true)
    (.executeMethod ua req)
    (prn (format "ua-get->string: req.uri=%s" (.getURI req)))
    req))

(defn ua-get->string [ua url & [params]]
  (.getResponseBodyAsString (apply ua-get ua url params)))

(defn #^String get->string [#^String url & [params]]
  (apply ua-get->string *ua* url params))

;; (get->string "http://google.com/")
;; (get->string "http://intranet.hmsonline.com/confluence/display/SWDEV/Home")


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

(defmacro with-http-client [[client & params] & body]
  (prn (format "with-http-client: generating, client=%s params=%s body=%s" client params body))
  `(let [params#   (second (kutils/parse-paired-arglist [~@params]))
         client#   (org.apache.commons.httpclient.HttpClient.)
         ~client   client#]
     (prn (format "with-http-client: params=%s" params#))
     (when (:user params#)
       (prn (format "with-http-client: setting credentials, :user=%s host=%s port=%s" 
                    (:user params#)
                    (:host params#)
                    (:port params#)))
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
