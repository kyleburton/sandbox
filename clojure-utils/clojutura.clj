(ns clojutura
  (:use [com.github.kyleburton.sandbox.utils]
        [clojure.contrib.duck-streams]
        [clojure.contrib.str-utils]))

(def *cobertura-project-data* (file->object ($HOME "personal/projects/sandbox/clojure-utils/cobertura.ser")))

;; hack for now, so it runs from ant
(defmacro mwith-out-writer
  "Opens a writer on f, binds it to *out*, and evalutes body."
  [f & body]
  `(with-open [stream# (writer ~f)]
     (binding [*out* stream#]
       ~@body)))

;; see: package net.sourceforge.cobertura.reporting.html.HTMLReport for an example

(def *report-dir* ($HOME "tmp/clojutura"))
(def *src-dir* ($HOME "personal/projects/sandbox/clojure-utils/src/clj"))

(mkdir *report-dir*)

(def *html-header*
     (format "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
           \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
<title>[Cojutura] Coverage Report</title>
<link title=\"Style\" type=\"text/css\" rel=\"stylesheet\" href=\"%s/css/main.css\" />
</head>
<body>" *report-dir*))


(def *html-footer* "</body></html>")

(defn make-style-sheet []
  (mkdir (format "%s/css" *report-dir*))
  (mwith-out-writer (format "%s/css/main.css" *report-dir*)
    (println ".covered-false {")
    (println " background: #F77;")
    (println "}")
    (println ".covered-true {")
    (println " background: #7F7;")
    (println "}")))


(defn gen-rep-for-package [project-data cover-info]
  (mwith-out-writer (format "%s/package-%s.html" *report-dir* (.getName cover-info))
    (print *html-header*)
    (print (format "<h2>%s</h2>" (.getName cover-info)))
    (println (format "<pre>"))
    (println (format "Line Coverage Rate: %s" (.getLineCoverageRate cover-info)))
    (println (format "     Lines Covered: %s" (.getNumberOfCoveredLines cover-info)))
    (println (format "Branch Coverage Rate: %s" (.getBranchCoverageRate cover-info)))
    (println (format "    Branches Covered: %s" (.getNumberOfCoveredBranches cover-info)))
    (println (format "</pre>"))
    (print *html-footer*)))

(defn cover-name->path [cover-info]
  (format "%s/files/%s.html" *report-dir* (.getName cover-info)))

(defn line-info-css-classes [lcover]
  (if (not lcover)
    ""
    (let [bn (bean lcover)]
      (str-join " "
                ;; {:covered false, :hits 0, :branchCoverageRate 1.0, :numberOfCoveredLines 0, :lineNumber 90, :class net.sourceforge.cobertura.coveragedata.LineData, :methodName "invoke", :conditionSize 0, :numberOfCoveredBranches 0, :methodDescriptor "(Ljava/lang/Object;)Ljava/lang/Object;", :numberOfValidLines 1, :numberOfValidBranches 0, :lineCoverageRate 0.0} 
                (map #(format "%s-%s" (name %) (bn %))
                     [:covered])))))


(defn format-clojure-line [line]
  (loop [line line [[pat rep] & repls] [["&" "&amp;"]
                                        [" " "&nbsp;"]
                                        ["<" "&lt;"]
                                        [">" "&gt;"]]]
    (if (not pat)
      line
      (recur (.replaceAll line pat rep)
             repls))))

(defn gen-rep-for-file [project-data file-info]
  (let [fname (cover-name->path file-info)]
    (.println System/err (format "gen-rep-for-file: fname=%s" fname))
    (mkdir (basename fname))
    (mwith-out-writer fname
      (print *html-header*)
      (print (format "<h2>%s</h2>" (.getName file-info)))
      (println (format "<pre>"))
      (println (format "Line Coverage Rate: %s"   (.getLineCoverageRate file-info)))
      (println (format "     Lines Covered: %s"   (.getNumberOfCoveredLines file-info)))
      (println (format "Branch Coverage Rate: %s" (.getBranchCoverageRate file-info)))
      (println (format "    Branches Covered: %s" (.getNumberOfCoveredBranches file-info)))
      (println (format "</pre><table>"))
      (let [fname (str *src-dir* "/" (.getName file-info))]
        (.println System/err (format "Looking for:%s" fname))
        (if (.exists (java.io.File. fname))
          (loop [lnum 1
                 [line & lines] (read-lines fname)]
            (if line
              (let [lcover (.getLineCoverage file-info lnum)]
                ;; {:covered false, :hits 0, :branchCoverageRate 1.0, :numberOfCoveredLines 0, :lineNumber 24, :class net.sourceforge.cobertura.coveragedata.LineData, :methodName "invoke", :conditionSize 0, :numberOfCoveredBranches 0, :methodDescriptor "(Ljava/lang/Object;)Ljava/lang/Object;", :numberOfValidLines 1, :numberOfValidBranches 0, :lineCoverageRate 0.0} 
                (println (format "<tr><td><code>%s</code></td><td class=\"%s\"><code>%s</code></td><!-- %s -->" 
                                 lnum
                                 (line-info-css-classes lcover)
                                 (format-clojure-line line)
                                 (if lcover (bean lcover)
                                     "")))
                (recur (+ lnum 1) lines))))))
      '(doseq [lnum (range 0 (.getNumberOfValidLines file-info))]
        (let [lcover (.getLineCoverage file-info lnum)]
          (if (.isValidSourceLineNumber file-info lnum)
            (println (format "[%s] lnum=%s valid=true  %s" (.getLineNumber lcover) lnum lcover))
            (println (format "lnum=%s valid=false %s" lnum lcover)))))
      (println (format "</pre>"))
      (print *html-footer*))))

(defn gen-package-list [project-data]
  (mwith-out-writer (str *report-dir* "/packages.html")  
    (print *html-header*)
    (print "<h1>[Cojutura] Coverage Report</h1>
<h2>Packages</h2>
<table width=\"100%\">")

    (doseq [class-info (.getClasses project-data)]
      (print (format "<tr><td><a href=\"package-%s.html\">%s</a></td></tr>" (.getName class-info) (.getName class-info)))
      (gen-rep-for-package project-data class-info))
    (print "</table>")
    (print "<h2>Source Files</h2><table>")
    (doseq [file-info (.getSourceFiles project-data)]
      (print (format "<tr><td><a href=\"files/%s.html\">%s</a></td></tr>" (.getName file-info) (.getName file-info)))
      (gen-rep-for-file project-data file-info))
    (print "</table>")
    (print *html-footer*)))

(defn gen-files-list [project-data]
  (mwith-out-writer (str *report-dir* "/files.html")  
    (print *html-header*)
    (print "<h1>[Cojutura] Coverage Report</h1>")
    (print "</table>")
    (print "<h2>Source Files</h2><table>")
    (doseq [file-info (.getSourceFiles project-data)]
      (print (format "<tr><td><a href=\"files/%s.html\">%s</a></td></tr>" (.getName file-info) (.getName file-info)))
      (gen-rep-for-file project-data file-info))
    (print "</table>")
    (print *html-footer*)))

;;(first (.getClasses *cobertura-project-data*))
;; (.containsInstrumentationInfo (first (.getSourceFiles *cobertura-project-data*)))
;; (.getLineCoverage (first (.getSourceFiles *cobertura-project-data*)) 1)

'(doseq [file-info (filter (fn [info]
                            (.contains (.getName info)
                                       "kyleburton"))
                          (.getSourceFiles *cobertura-project-data*))]
  (prn (format "%s: # lines: %s" (.getName file-info) (.getNumberOfValidLines file-info)))
  (doseq [lnum (range 0 (.getNumberOfValidLines file-info))]
    (if (.isValidSourceLineNumber file-info lnum)
      (prn (format "lnum=%s valid=true" lnum))
      (prn (format "lnum=%s valid=false" lnum)))))

;; (map (fn [info]
;;        [(.getName info)
;;         (.getNumberOfValidLines info)])
;;      (.getSourceFiles *cobertura-project-data*))


;;     (.getLineCoverage file-info
;;                       lnum)

(make-style-sheet)
(gen-files-list *cobertura-project-data*)


