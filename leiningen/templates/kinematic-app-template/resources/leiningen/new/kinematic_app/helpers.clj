(ns {{name}}.helpers
  (:require
   [clj-time.core   :as time]
   [clj-time.coerce :as tcoerce]
   [clj-time.format :as tformat])
  (:import
   [org.mindrot.jbcrypt BCrypt]
   [java.sql Timestamp]
   [java.io PrintWriter]
   [org.joda.time DateTime]
   [org.joda.time.format ISODateTimeFormat]))


(defn string->date [^String s]
  (when (string? s)
    (->
     (tformat/parse (:date-time tformat/formatters) s)
     .toDate)))

(defn date->string [^java.util.Date d]
  (tcoerce/from-date d))

