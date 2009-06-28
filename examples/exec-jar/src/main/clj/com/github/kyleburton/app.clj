(ns com.github.kyleburton.app
  (:gen-class)
  (:use [com.github.kyleburton.sandbox.web :as kweb]
        [com.github.kyleburton.sandbox.landmark-parser :as lp]
        [clojure.contrib.str-utils :as str-utils]))

(defn fetch-page [terms]
  (let [page (kweb/get->string (format "http://www.google.com/search?q=%s&ie=utf-8&oe=utf-8&aq=t&rls=org.mozilla:en-US:official&client=firefox-a" (str-utils/str-join "+" terms)))]
    (doseq [link (filter #(.contains % "class=l") (lp/html->anchors page))]
      (let [href (lp/anchor->href link)
            text (kweb/strip-html (lp/anchor->body link))]
        (println text)
        (println href)
        (println "\n")))))

(defn show-help []
  "app term [term2 [term3 ...]]

Performs a Google Search for the given terms

")

(defn -main [& terms]
  (cond (empty? terms)
        (show-help)
        true
        (fetch-page terms)))


