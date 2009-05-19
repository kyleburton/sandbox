;; Code for scraping down my livejoural posts -- I'm converting to my
;; own site, built via Jekyll as of May 2009...

(ns com.github.kyleburton.sandbox.lj
  (:use [com.github.kyleburton.sandbox.web :as web]
        [com.github.kyleburton.sandbox.landmark-parser :as lp]
        [com.github.kyleburton.sandbox.utils :as kutils]
        [clojure.contrib.duck-streams :as ds]))

;; (def *ljurl* "http://kyle-burton.livejournal.com/")
;; (def *ljurl* "http://kyle-burton.livejournal.com/?skip=20")
(def *ljurl* "http://kyle-burton.livejournal.com/?skip=40")
(def *jekyll-root* (kutils/$HOME "personal/projects/this-blog"))

(def main-page (web/get->string *ljurl*))

(defn get-posts [html]
  (lp/extract-all-from html
                       '(:ft "subj-link" :rp "<table")
                       '(:fp "post comment" :fp "</table>"))  )

(def *months* {"Jan"  1
               "Feb"  2
               "Mar"  3
               "Apr"  4
               "May"  5
               "Jun"  6
               "Jul"  7
               "Aug"  8
               "Sep"  9
               "Oct" 10
               "Nov" 11
               "Dec" 12})

(defn str->mon [s]
  (*months*
   (first (filter #(.startsWith (.toLowerCase s) (.toLowerCase %))
                  (keys *months*)))))

(defn parse-lj-date-time [date]
  (let [[date time]                    (seq (.split (web/strip-html (.replaceAll date "[\\[\\]]" "")) "\\|" 2))
        [month-name day-of-month year] (kutils/re-find-first #"\s*(\S+)\s+(\S+),\s+(\S+)" date)
        [hour min am-pm]               (kutils/re-find-first #"\s*(\S+):(\S+)\s+(\S+)" time)]
    {:year          (Integer/parseInt year)
     :month         (str->mon month-name)
     :day           (Integer/parseInt (.replaceAll day-of-month "[^0-9]+" ""))
     :full-date     date
     :am-pm         am-pm
     :hour          (Integer/parseInt hour)
     :hour-military (if (= "pm" am-pm)
                      (+ 12 (Integer/parseInt hour))
                      (Integer/parseInt hour))
     :min           (Integer/parseInt min)
     :sec           0
     :full-time     time}))

(defn parse-post [post]
  (let [parser (lp/make-parser post)]
    {:title    (lp/extract parser '(:ft "subj-link" :fp ">") '(:ft "<"))
     :date     (parse-lj-date-time (lp/extract parser '(:ft "<td " :fp ">")      '(:ft "</td")))
     :tags     (if (not (= -1 (.indexOf post "<b>Tags</b>")))
                 (vec (.split (lp/extract-from post
                                               '(:ft "Tags" :fp "href=" :fp ">")
                                               '(:ft "<"))
                              "\\s+"))
                 [])
     :body     (lp/extract parser '(:fp "<td colspan=")          '(:fp "'comments'" :rp "</td>" ))}))


;; (:date (parse-post (first (get-posts main-page))))

(defn title->safename [title]
  (.trim
   (.replaceAll
    (.replaceAll (.toLowerCase title) "[.]+" "")
    "[^a-zA-Z0-9]+" "-")))

(defn post->file-name [post]
  (let [{:keys [date title tags]} post
        {:keys [year month day]}  date
        ]
    (format "%04d-%02d-%02d-%s.textile"
            year month day (title->safename title))))

(defn post->full-pathanme [post]
  (str *jekyll-root* "/site/_posts/" (post->file-name post)))

(doseq [post (map parse-post (get-posts main-page))]
  (with-open [out (ds/writer (post->full-pathanme post))]
    (binding [*out* out]
      (println "---")
      (println "layout: default")
      (println (str "title: " (:title post)))
      (println "---")
      (println (:body post)))))

;; (post->file-name (parse-post (nth (get-posts main-page) 1)))
;; "2008-10-21-cloud-con-east-notes.textile"

;; (ds/spit (kutils/$HOME "/tmp/lj.html")
;;          (first (get-posts main-page)))


;; (apply concat (map :tags (for [post (get-posts main-page)]
;;                            (parse-post post))))