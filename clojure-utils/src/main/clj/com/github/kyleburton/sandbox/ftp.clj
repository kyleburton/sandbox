;; simple wrappers for jakarta's ftp client

;; TODO: keep checking reply codes, throw exception if not OK

(ns com.github.kyleburton.sandbox.ftp
  (:import (org.apache.commons.net.ftp FTP FTPClient)
           (java.net URL))
  (:use [ clojure.contrib.str-utils :as str]))

(defmulti open "Open an ftp connection." class)

(defmethod open String [s]
  (open (java.net.URL. s)))

(defmethod open URL [url]
  (let [client (FTPClient.)]
    (.connect client
              (.getHost url)
              (if (= -1 (.getPort url))
                (.getDefaultPort url)
                (.getPort url)))
    client))

(defmacro with-ftp [[client url & extra-bindings] & body]
  `(let [u# (URL. ~url) 
         ~client (open u#)
         res# (atom nil)
         ~@extra-bindings]
     (if (.getUserInfo u#)
       (let [[uname# pass#] (.split (.getUserInfo u#) ":" 2)]
         (.login ~client uname# pass#)))
     (.changeWorkingDirectory ~client (.getPath u#))
     (.setFileType ~client FTP/BINARY_FILE_TYPE)
     (reset! res# 
            (do
              ~@body))
     (.disconnect ~client)
     @res#))

(defn list-all [url]
  (with-ftp [client url]
    (map #(.getName %) (.listFiles client))))

(defn list-files [url]
  (with-ftp [client url]
    (map #(.getName %) (filter #(.isFile %) (.listFiles client)))))

(defn list-directories [url]
  (with-ftp [client url]
    (map #(.getName %) (filter #(.isDirectory %) (.listFiles client)))))

(defn retrieve-file [url fname & [local-file]]
  (with-ftp [client url]
    (if local-file
      (with-open [outstream (java.io.FileOutputStream. local-file)]
        (.retrieveFile client fname outstream))
      (.retrieveFile client fname))))

;; (retrieve-file "ftp://anonymous:user%40host.com@ftp2.census.gov/geo/tiger/TIGER2008/42_PENNSYLVANIA/" "tl_2008_42_place.zip" "/home/mortis/tl_2008_42_place.zip") 
;; (list-files "ftp://anonymous:user%40host.com@ftp2.census.gov/geo/tiger/TIGER2008/42_PENNSYLVANIA/")