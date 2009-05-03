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
     (prn "u=" u#)
     (if (.getUserInfo u#)
       (let [[uname# pass#] (.split (.getUserInfo u#) ":" 2)]
         (prn "uname=" uname#)
         (prn "pass=" pass#)
         (.login ~client uname# pass#)))
     (prn "path=" (.getPath u#))
     (.changeWorkingDirectory ~client (.getPath u#))
     (reset! res# 
            (do
              ~@body))
     (prn "disconnecting: " ~client)
     (.disconnect ~client)
     @res#))

