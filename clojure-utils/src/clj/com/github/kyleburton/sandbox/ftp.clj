(ns com.github.kyleburton.sandbox.ftp
  (:import (org.apache.commons.net.ftp FTP FTPClient)
           (java.net URL))
  (:use [ clojure.contrib.str-utils :as str]))

(defmulti open class)

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
     (reset! res# 
            (do
              ~@body))
     (.disconnect ~client)
     @res#))

