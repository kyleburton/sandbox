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
         (drop 1 (re-matches #".+HREF=\"([^\"]+)\".*?ADD_DATE=\"([^\"]+)\".*?TAGS=\"([^\"]*)\".*?>([^<]+)</A>.*" line))]
     {:link        link
      :add-date    add-date
      :tags        (vec (.split tags ","))
      :description description})
   (catch Exception ex
     (do
       (prn "stacktrace:")
       (.printStackTrace ex)
       (prn "...stacktrace")
       (prn (format "unable to parse line='%s' ex=%s" line ex))
       {}))))

;; (parse-line (first (filter #(.startsWith %1 "<DT>") (line-seq (ds/reader *bookmarks-file*)))))

;; (map :tags (take 3 (map parse-line (filter #(.startsWith %1 "<DT>") (line-seq (ds/reader *bookmarks-file*))))))

(defn dump-to-tab [#^String fname]
  (with-open [out (ds/writer fname)]
    (binding [*out* out]
      (println (str-utils/str-join "\t" ["link" "add_date" "description" "tag"]))
      (dorun
       (doseq [bmrk (map parse-line (filter #(.startsWith %1 "<DT>") (line-seq (ds/reader *bookmarks-file*))))
               tag (:tags bmrk)]
         (println (str-utils/str-join "\t" [(:link bmrk) (:add-date bmrk) (:description bmrk) tag])))))))


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

(defn build-tag-map [file]
  (let [[header & lines] 
        (map #(seq (.split % "\t"))
             (line-seq (ds/reader file)))
        res (java.util.HashMap.)]
    (doseq [[link add-date description tag] lines]
      (if (not (empty? tag))
        (.put res tag (doto (or (.get res tag) (java.util.ArrayList.))
                        (.add link)))))
    res))

(def *links-by-tag* (build-tag-map (proj-file "delicious.tab")))

(defn reverse-map [m]
  (let [res (java.util.HashMap.)]
    (doseq [[tag urls] (.entrySet *links-by-tag*)
            url urls]
      (.put res url (doto (or (.get res url) (java.util.ArrayList.))
                      (.add tag))))
    res))

(def *tags-by-link* (reverse-map *links-by-tag*))

(first (reverse (sort-by #(.size (.getValue %)) (seq *links-by-tag*))))


