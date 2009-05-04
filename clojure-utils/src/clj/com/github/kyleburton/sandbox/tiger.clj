(ns com.github.kyleburton.sandbox.tiger
  (:import (org.apache.commons.net.ftp FTP FTPClient))
  (:require [com.github.kyleburton.sandbox.ftp :as ftp]
            [com.github.kyleburton.sandbox.landmark-parser :as lparse]
            [com.github.kyleburton.sandbox.utils :as kutils])
  (:use [clojure.contrib.str-utils :as str]
        [clojure.contrib.fcase :only (case)]))

(def *tiger-ftp-url* "ftp://anonymous:user%40host.com@ftp2.census.gov/geo/tiger/TIGER2008/")

(ftp/list-all *tiger-ftp-url*)
(ftp/list-files *tiger-ftp-url*)
(ftp/list-directories *tiger-ftp-url*)

'(let [data-dir (kutils/expand-file-name "~/data-sets/tiger-line/data")
        state  "PENNSYLVANIA"
        url    (str *tiger-ftp-url* "42_" state)]
  (.mkdirs (java.io.File. data-dir state))
  (dorun (doseq [file (ftp/list-files url)]
           (let [local-file (str data-dir "/" state "/" file)]
             (if (not (.exists (java.io.File. local-file)))
               (do
                 (prn (format "fetch url %s => %s" url file local-file))
                 (ftp/retrieve-file url
                                    file
                                    local-file))
               (prn (format "already have: %s" local-file)))))))

'(let [data-dir (kutils/expand-file-name "~/data-sets/tiger-line/data")
        state  "PENNSYLVANIA/42045_Delaware_County"
        url    (str *tiger-ftp-url* "42_" state)]
  (.mkdirs (java.io.File. data-dir state))
  (dorun (doseq [file (ftp/list-files url)]
           (let [local-file (str data-dir "/" state "/" file)]
             (if (not (.exists (java.io.File. local-file)))
               (do
                 (prn (format "fetch url %s => %s" url file local-file))
                 (ftp/retrieve-file url
                                    file
                                    local-file))
               (prn (format "already have: %s" local-file)))))))

;; (ftp/retrieve-file *tiger-ftp-url* "tl_2008_us_nectadiv.zip" "/tmp/tl_2008_us_nectadiv.zip")

(defmulti enumeration->seq class)

(defmethod enumeration->seq java.util.Enumeration [enum]
  (loop [res []]
    (if (.hasMoreElements enum)
      (recur (conj res (.nextElement enum)))
      (seq res))))

(defmethod enumeration->seq java.util.zip.ZipFile [zfile]
  (enumeration->seq (.entries zfile)))

;; (enumeration->seq (.entries (java.util.zip.ZipFile. (kutils/expand-file-name "~/data-sets/tiger-line/data/PENNSYLVANIA/tl_2008_42_bg00.zip"))))
;; (enumeration->seq (java.util.zip.ZipFile. (kutils/expand-file-name "~/data-sets/tiger-line/data/PENNSYLVANIA/tl_2008_42_bg00.zip")))

;; (map #(.getName %) (enumeration->seq (java.util.zip.ZipFile. (kutils/expand-file-name "~/data-sets/tiger-line/data/PENNSYLVANIA/tl_2008_42_bg00.zip"))))

;; (def dbf (org.xBaseJ.DBF. (kutils/expand-file-name "/data-sets/tiger-line/data/tmp/tl_2008_us_state.dbf")))

;; (doseq [ii (range 1 (+ 1 (.getFieldCount dbf)))]
;;   (prn (format "field[%d]: %s" ii (.getName (.getField dbf ii)))))

;; (.getRecordCount dbf)


(defn fields [#^org.xBaseJ.DBF dbf]
  (for [ii (range 1 (+ 1 (.getFieldCount dbf)))]
    (.getField dbf ii)))

(defn for-each-row [fn #^org.xBaseJ.DBF dbf]
  (let [fields (fields dbf)]
    (dotimes [recno (.getRecordCount dbf)]
      (.read dbf)
      (fn recno fields))))

;; (with-open [dbf (org.xBaseJ.DBF. (kutils/expand-file-name "~/data-sets/tiger-line/data/tmp/tl_2008_us_state.dbf"))]
;;   (for-each-row (fn [recno fields]
;;                   (doseq [field fields]
;;                     (prn (format "recno[%d] [%s/%s]=%s"
;;                                  recno
;;                                  (.getType field)
;;                                  (.getName field)
;;                                  (.trim  (.get field))))))
;;                 dbf))

(defn field-names [#^org.xBaseJ.DBF dbf]
  (map #(.getName %) (fields dbf)))

;; (field-names (org.xBaseJ.DBF. (kutils/expand-file-name "~/data-sets/tiger-line/data/tmp/tl_2008_us_state.dbf")))

(defn dbf->tabfile [dbfile tabfile]
  (with-open [dbf (org.xBaseJ.DBF. dbfile)]
    (with-open [outp (java.io.PrintWriter. tabfile)]
      (binding [*out* outp]
        (println (str/str-join "\t" (field-names dbf)))
        (for-each-row 
         (fn [recno fields]
           (println (str/str-join "\t" (map #(.trim (.get %)) fields))))
         dbf)))))

(dbf->tabfile 
 (kutils/expand-file-name "~/data-sets/tiger-line/data/tmp/tl_2008_us_state.dbf") 
 (kutils/expand-file-name  "~/us_state.tab"))

(dbf->tabfile 
 (kutils/expand-file-name "~/data-sets/tiger-line/data/tmp/tl_2008_us_county.dbf") 
 (kutils/expand-file-name "~/us_county.tab"))


;; (def ua (org.apache.commons.httpclient.HttpClient.))
;; (def req (org.apache.commons.httpclient.methods.GetMethod. "http://www.dbase.com/knowledgebase/int/db7_file_fmt.htm"))
;; (def resp (.executeMethod ua req))

;; #'req


(def type-descr
     (for [row (map lparse/row->cells
                    (lparse/table-rows
                     (lparse/extract-from
                      (.getResponseBodyAsString req)
                      '((:fp "Storage of dBASE")
                        (:fp "</tr>"))
                      '((:ft "</table")))))]
       (map #(let [cell (lparse/extract-from % '((:ft "</font>")
                                                 (:rt ">"))
                                             '((:ft "</font>")))]
               (.replaceAll
                (.replaceAll cell "[\r\n]+" "")
                " +" " "))
            row)))

(print (str/str-join "\n" (map #(str/str-join "\t" %) type-descr)))

;; B	Binary, a string	10 digits representing a .DBT block number. 
;;                              The number is stored as a string, right justified and padded with blanks.
;; C	Character	All OEM code page characters - padded with blanks to the width of the field.
;; D	Date	8 bytes - date stored as a string in the format YYYYMMDD.
;; N	Numeric	Number stored as a string, right justified, and padded with blanks to the width of the field.&nbsp;
;; L	Logical	1 byte - initialized to 0x20 (space) otherwise T or F.
;; M	Memo, a string	10 digits (bytes) representing a .DBT block number. The number is stored as a 
;;                               string, right justified and padded with blanks.
;; @	Timestamp	8 bytes - two longs, first for date, second for time.&nbsp; 
;;                      The date is the number of days since&nbsp; 01/01/4713 BC. 
;;                      Time is hours * 3600000L + minutes * 60000L + Seconds * 1000L
;; I	Long	4 bytes. Leftmost bit used to indicate sign, 0 negative.
;; +	Autoincrement	Same as a Long
;; F	Float	Number stored as a string, right justified, and padded with blanks to the width of the field.&nbsp;
;; O	Double	8 bytes - no conversions, stored as a double.
;; G	OLE	10 digits (bytes) representing a .DBT block number. 
;;              The number is stored as a string, right justified and padded with blanks.nil
