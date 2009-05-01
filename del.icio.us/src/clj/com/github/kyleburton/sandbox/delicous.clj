(ns com.github.kyleburton.sandbox.delicious
  (:require [clojure.contrib.duck-streams :as ds])
  (:require [ clojure.contrib.str-utils :as str-utils]))

(defn basename [fnane]
  (.getParent (java.io.File. *file*)))

(def *this-dir* (basename *file*))


(defn relative-file [fname]
  (if (.startsWith fname "/")
    (str *this-dir* fname)
    (str *this-dir* "/" fname)))

(def *proj-root*
     (loop [here (ds/file *this-dir*)]
       (if (= "src" (.getName here))
         (.getParent here)
         (recur (ds/file (.getParent here))))))

(defn proj-file [fname]
  (.replaceAll (str *proj-root* "/" fname)
               "//" "/"))

(def *bookmarks-file* (proj-file "/data/delicious-20090429.htm"))

;; "<DT><A HREF=\"http://hopper.squarespace.com/blog/2008/6/22/introducing-shovel-an-amqp-relay.html\" LAST_VISIT=\"1239821460\" ADD_DATE=\"1239821460\" TAGS=\"shovel,erlang,rabbit-mq,messaging,amqp\">No Clumps, Fisheyes or Microgels  -  Blog   - Introducing Shovel: An AMQP Relay</A>"
(defn parse-line [#^String line]
  (try
   (let [[link add-date tags description] 
         (drop 1 (re-matches #".+HREF=\"([^\"]+).*?ADD_DATE=\"([^\"]+).*?TAGS=\"([^\"]+).*?>([^<]+)</A>.*" line))]
     {:link        link
      :add-date    add-date
      :tags        (vec (.split tags ","))
      :description description})
   (catch Exception ex
     (prn (format "unable to parse line='%s' ex=%s" line ex)))))

;; (parse-line (first (filter #(.startsWith %1 "<DT>") (line-seq (ds/reader *bookmarks-file*)))))

;; (map :tags (take 3 (map parse-line (filter #(.startsWith %1 "<DT>") (line-seq (ds/reader *bookmarks-file*))))))

(defn dump-to-tab [#^String fname]
  (with-open [out (ds/writer fname)]
    (binding [*out* out]
      (dorun
       (doseq [bmrk (map parse-line (filter #(.startsWith %1 "<DT>") (line-seq (ds/reader *bookmarks-file*))))
               tag (:tags bmrk)]
         (println (str-utils/str-join "\t" [(:link bmrk) (:add-date bmrk) (:description bmrk) tag])))))))



(dump-to-tab (proj-file "delicious.tab"))

(map parse-line (filter #(.startsWith %1 "<DT>") (line-seq (ds/reader *bookmarks-file*))))

