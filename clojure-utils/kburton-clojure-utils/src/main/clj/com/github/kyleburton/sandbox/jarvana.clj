(ns com.github.kyleburton.sandbox.jarvana
  (:use
   [clojure.xml :only [parse]]
   [com.github.kyleburton.clj-xpath :only [$x $x:tag $x:tag* $x:text $x:text*]])
  (:require
   [com.github.kyleburton.sandbox.log4j :as log]
   [clojure.contrib.duck-streams :as ds]

   [clojure.contrib.str-utils :as str]))


(def *log* (log/get-logger *ns*))


;; search_type=class   params[java_class]
;; search_type=project params[project]
;; search_type=content params[content]
(def *url* "http://www.jarvana.com/jarvana/search-xml/1.0?search_type=class&java_class=org.apache.commons.lang.StringUtils&start=0")

(def *config* {
               :search-url "http://www.jarvana.com/jarvana/search-xml"
               :version    "1.0"
               })

(defn url-for-class [class]
  (format "%s/%s?search_type=class&java_class=%s&start=0"
          (:search-url *config*)
          (:version *config*)
          class))

(defn url-for-project [project]
  (format "%s/%s?search_type=project&project=%s&start=0"
          (:search-url *config*)
          (:version *config*)
          project))

(defn url-for-content [content]
  (format "%s/%s?search_type=content&content=%s&start=0"
          (:search-url *config*)
          (:version *config*)
          content))

;; (def *xml* (ds/slurp* *url*))

(defn find-by-class [class]
  (ds/slurp* (url-for-class class)))

(defn find-by-project [project]
  (ds/slurp* (url-for-project project)))

(defn find-by-content [content]
  (ds/slurp* (url-for-content content)))

(comment

  (def some-xml (find-by-project "clojure"))
  (def the-doc (com.github.kyleburton.clj-xpath/xml->doc some-xml))

  ($x:tag* "/*" the-doc)
  ($x:tag* "//*" the-doc)
  ($x:text "/projectSearchResults/totalHits" the-doc)
  (first ($x "/projectSearchResults/results" the-doc))
)



