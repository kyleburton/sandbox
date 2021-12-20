(ns com.github.kyleburton.sandbox.log4j
  (:import
   [org.apache.log4j Logger PropertyConfigurator]
   [org.apache.commons.logging Log LogFactory])
  (:require [com.github.kyleburton.sandbox.utils :as kutils])
  (:use [clojure.contrib.str-utils :as str]))

(def *log4j-properties* (str (kutils/$HOME ".clojure/log4j.properties")))

(def *configured* (atom false))

(defn reset-configuration! []
  (reset! *configured* false))

(defn ensure-configured []
  (if @*configured*
    true
    (do
      (PropertyConfigurator/configureAndWatch *log4j-properties* 1000)
      (reset! *configured* true))))

(defn get-logger [category]
  (LogFactory/getLog (str category)))


(defmacro trace [& body] `(if (.isTraceEnabled ~'*log*) (.trace ~'*log* (str ~@body))))
(defmacro debug [& body] `(if (.isDebugEnabled ~'*log*) (.debug ~'*log* (str ~@body))))
(defmacro info  [& body] `(if (.isInfoEnabled  ~'*log*) (.info  ~'*log* (str ~@body))))
(defmacro warn  [& body] `(if (.isWarnEnabled  ~'*log*) (.warn  ~'*log* (str ~@body))))
(defmacro error [& body] `(if (.isErrorEnabled ~'*log*) (.error ~'*log* (str ~@body))))
(defmacro fatal [& body] `(if (.isFatalEnabled ~'*log*) (.fatal ~'*log* (str ~@body))))

(defmacro tracef [fmt & args] `(if (.isTraceEnabled ~'*log*) (trace (format ~fmt ~@args))))
(defmacro debugf [fmt & args] `(if (.isDebugEnabled ~'*log*) (debug (format ~fmt ~@args))))
(defmacro infof  [fmt & args] `(if (.isInfoEnabled  ~'*log*) (info  (format  ~fmt ~@args))))
(defmacro warnf  [fmt & args] `(if (.isWarnEnabled  ~'*log*) (warn  (format  ~fmt ~@args))))
(defmacro errorf [fmt & args] `(if (.isErrorEnabled ~'*log*) (error (format ~fmt ~@args))))
(defmacro fatalf [fmt & args] `(if (.isFatalEnabled ~'*log*) (fatal (format ~fmt ~@args))))




