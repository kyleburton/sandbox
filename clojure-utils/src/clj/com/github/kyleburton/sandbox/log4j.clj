(ns com.github.kyleburton.sandbox.log4j
  (:import
   (org.apache.log4j Logger PropertyConfigurator))
  (:require [com.github.kyleburton.sandbox.utils :as kutils])
  (:use [clojure.contrib.str-utils :as str]))

(def *log4j-properties* (str (kutils/$HOME ".clojure/log4j.properties")))

(def *configured* (atom false))

(defn reset-configuration! []
  (reset! *configured* false))

;; (reset-configuration!)

(defn ensure-configured []
  (if @*configured*
    true
    (do
      (PropertyConfigurator/configureAndWatch *log4j-properties* 1000)
      (reset! *configured* true))))

(defn logger [category]
  (ensure-configured)
  (Logger/getLogger category))

;; TODO: wrap in (if LOG.isDebug) to prevent unnecessary evaluation of arguments
(defmacro LOG []
  `(do (def ~'*log* (logger (.toString ~'*ns*)))
       (defn ~'log-fatal [& args#] (.fatal ~'*log* (str args#)))
       (defn ~'log-error [& args#] (.error ~'*log* (str args#)))
       (defn ~'log-warn  [& args#] (.warn  ~'*log* (str args#)))
       (defn ~'log-info  [& args#] (.info  ~'*log* (str args#)))
       (defn ~'log-debug [& args#] (.debug ~'*log* (str args#)))
       (defn ~'log-trace [& args#] (.trace ~'*log* (str args#)))))

