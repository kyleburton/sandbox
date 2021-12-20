;; Maven 2 utilities
;;
;;
;;
;; (map :name (mvn-plugins))

;; => ("clean" "compiler" "deploy" "install" "resources" "site"
;;     "surefire" "verifier" "ear" "ejb" "jar" "rar" "war" "shade"
;;     "changelog" "changes" "checkstyle" "clover" "doap" "docck"
;;     "javadoc" "jxr" "pmd" "project-info-reports" "surefire-report"
;;     "ant" "antrun" "archetype" "assembly" "dependency" "enforcer" "gpg"
;;     "help" "invoker" "one" "patch" "plugin" "release" "reactor"
;;     "remote-resources" "repository" "scm" "source" "stage" "eclipse"
;;     "idea")

;;
;; (map :name (mvn-plugin-goals "release"))
;; => ("release:branch" "release:clean" "release:help" "release:perform"
;;     "release:prepare" "release:rollback" "release:stage")
;; 
;;;; (map :name (mvn-plugin-goals "dependency"))
;; (filter #(.contains (:name %) "copy") (mvn-plugin-goals "dependency"))

(ns com.github.kyleburton.sandbox.mvn
  (:require [com.github.kyleburton.sandbox.utils :as kutils]
            [com.github.kyleburton.sandbox.landmark-parser :as lparse]
            [com.github.kyleburton.sandbox.web :as web]
            [clojure.contrib.duck-streams :as ds]
            [clojure.contrib.str-utils :as str])
  (:use     [com.github.kyleburton.sandbox.memoize :only [def-once-only]]))

(def *mvn-plugins-url* "http://maven.apache.org/plugins/")

(def-once-only mvn-plugins-html [& [url]]
  (web/get->string (or url *mvn-plugins-url*)))

(def-once-only mvn-plugins-grid []
  (filter #(and (not (empty? %)) (.contains (nth % 0) "href"))
   (lparse/html-table->matrix 
    (first
     (filter
      ;; NB: this hard-coded string might go away at some point...
      #(.contains % "Clean up after the build.")
      (lparse/html->tables (mvn-plugins-html)))))))

(def-once-only mvn-plugins []
  (map (fn [row]
         {
          :name              (.trim (web/strip-html (nth row 0)))
          :url               (first (lparse/html->links (nth row 0)))
          :version           (nth row 1)
          :release-date      (nth row 2)
          :description       (nth row 3)
          :source-repo       (first (lparse/html->links (nth row 4)))
          :issue-tracking    (first (lparse/html->links (nth row 5)))
          })
       (mvn-plugins-grid)))

;; (map :name (mvn-plugins))

(defn mvn-plugin-goals [plugin-name]
  (let [entry (first (filter #(= plugin-name (:name %))
                             (mvn-plugins)))
        url (str "http://maven.apache.org" (:url entry))
        plugin-page (web/memoized-get->string url)
        goal-url (str url (lparse/html-find-link-with-body plugin-page "Goal"))
        goal-page (web/memoized-get->string goal-url)
        goal-table (drop 1 (lparse/html-table->matrix
                            (first (filter #(.contains % "Goal") 
                                           (lparse/html->tables goal-page)))))]
    (map (fn [row]
           {
            :name        (.trim (web/strip-html (nth row 0)))
            :url         (first (lparse/html->links (nth row 0)))
            :plugin-url  goal-url
            :description (nth row 1);(web/html-decode (nth row 1))
            })
         goal-table)))

;; (filter  (mvn-plugin-goals "dependency"))
;; (map :plugin-url (mvn-plugin-goals "dependency"))
;; (filter #(.contains (:name %) "dependency") (mvn-plugins))