(ns com.github.kyleburton.sandbox.delicious
  (:require [clojure.contrib.duck-streams :as ds]
            [clojure.contrib.str-utils :as str-utils]
            [com.github.kyleburton.sandbox.landmark-parser :as lparse]
            [com.github.kyleburton.sandbox.utils :as kutils]
            [com.github.kyleburton.sandbox.web :as web]))

(def *bookmarks-file* 
     (str (kutils/$HOME "/personal/projects/sandbox/del.icio.us/data/delicious-20090429.htm")))

(def *bookmarks*
     (lparse/html->anchors (slurp *bookmarks-file*)))

;; "<DT><A HREF=\"http://hopper.squarespace.com/blog/2008/6/22/introducing-shovel-an-amqp-relay.html\" LAST_VISIT=\"1239821460\" ADD_DATE=\"1239821460\" TAGS=\"shovel,erlang,rabbit-mq,messaging,amqp\">No Clumps, Fisheyes or Microgels  -  Blog   - Introducing Shovel: An AMQP Relay</A>"
(defn parse-bookmark [#^String bkmrk]
  (let [bkmrk   (if (nil? bkmrk) "" bkmrk)
        tag-str (first (kutils/re-find-first #"TAGS=\"([^\"]+)\"" bkmrk))
        tags    (if (or (empty? tag-str) (nil? tag-str))
                  nil
                  (vec (.split tag-str ",")))]
    {:name       (.trim (web/strip-html bkmrk))
     :url        (first (kutils/re-find-first #"HREF=\"([^\"]+)\"" bkmrk))
     :last-visit (first (kutils/re-find-first #"LAST_VISIT=\"([^\"]+)\"" bkmrk))
     :add-date   (first (kutils/re-find-first #"ADD_DATE=\"([^\"]+)\"" bkmrk))
     :tags       tags}))

(defn dump-to-tab [#^String fname]
  (kutils/with-stdout-to-file 
   fname
   (println (str-utils/str-join "\t" ["url" "add_date" "last_visit" "description" "tag"]))
   (dorun
    (doseq [bmrk (map parse-bookmark *bookmarks*)
            tag (:tags bmrk)]
      (println (str-utils/str-join "\t"
                                   [(:url bmrk) (:add-date bmrk) (:last-visit bmrk) (:description bmrk) tag]))))))


;; (dump-to-tab (str (kutils/$HOME "del.tab")))

(defn tab-view [file]
  (let [lines (line-seq (ds/reader file))
        columns (.split (first lines)
                        "\t")]
    (loop [lines (rest lines)
           lnum 1]
      (if (empty? lines)
        true
        (do
          (println (format "[% 4d]--------------------------------" lnum))
          (let [row (.split (first lines) "\t")]
            (dotimes [ii (count row)]
                     (println (format "%15s: %s" 
                                      (nth columns ii)
                                      (nth row ii)))))
          (recur (rest lines) (inc lnum)))))))


