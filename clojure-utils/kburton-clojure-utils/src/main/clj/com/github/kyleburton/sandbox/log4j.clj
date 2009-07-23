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

;; (reset-configuration!)

(defn ensure-configured []
  (if @*configured*
    true
    (do
      (PropertyConfigurator/configureAndWatch *log4j-properties* 1000)
      (reset! *configured* true))))

(defn logger [category]
  ;; TODO: move the ensure out of here, initialization should be a
  ;; concern handled outside this moudle...
  (ensure-configured)
  (Logger/getLogger category))

;; TODO: wrap the expansions with if(Log.isDebug){...} to avoid
;; argument evaluation when logging is otherwise disabled, keep that
;; hidden in behind the macros...
(defmacro LOG []
  `(do (def ~'*log* (logger (.toString ~'*ns*)))
       (defn ~'log-fatal [& args#] (.fatal ~'*log* (str args#)))
       (defn ~'log-error [& args#] (.error ~'*log* (str args#)))
       (defn ~'log-warn  [& args#] (.warn  ~'*log* (str args#)))
       (defn ~'log-info  [& args#] (.info  ~'*log* (str args#)))
       (defn ~'log-debug [& args#] (.debug ~'*log* (str args#)))
       (defn ~'log-trace [& args#] (.trace ~'*log* (str args#)))))
