(ns com.github.kyleburton.code-gen
  (:import [org.apache.commons.io IOUtils])
  (:require
   [com.github.kyleburton.clj-xpath :as xp])
  (:gen-class))

;; this code will be invoked during maven's generate-sources phase

(def *builder-package* "com.github.kyleburton.musicLibrary.builders")
(def *project-basedir* (System/getProperty "project.basedir"))
(def *music-lib-xsd*   (str (System/getProperty "project.basedir") "/src/main/resources/schema/music.xsd"))

(defn string-template [tmpl params]
  (loop [res tmpl
         [k & ks] (keys params)]
    (if k
      (let [pat (format "\\{\\{%s\\}\\}" (name k))]
        (recur (.replaceAll res pat (.toString (params k pat)))
          ks))
      res)))

(defn resource-as-string [res-url]
  (with-open [istr (.getResourceAsStream (.getClass string-template) res-url)]
    (IOUtils/toString istr)))


(defn package-directory [package]
  (.replaceAll package "\\." "/"))

(defn generated-code-directory []
  (format "%s/src/main/generated/java/%s"
          *project-basedir*
          (package-directory *builder-package*)))

(defn -main [& args]
  (println (format "GENERATE SOURCES from %s" *music-lib-xsd*)))



