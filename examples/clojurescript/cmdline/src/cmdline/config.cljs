(ns cmdline.config
  (:require
   [schema.core :as s :include-macros true]
   [cljs.core.async :as async :refer [chan put! <! <!! close!]]))


(def fs          (js/require "fs"))
(def line-reader (js/require "line-reader"))

(def config (atom {}))

(s/defn read-config-file [fname :- s/Str]
  (js/JSON.parse (.readFileSync fs fname)))

(s/defn load-config! [fname :- s/Str]
  (reset! config (read-config-file fname)))