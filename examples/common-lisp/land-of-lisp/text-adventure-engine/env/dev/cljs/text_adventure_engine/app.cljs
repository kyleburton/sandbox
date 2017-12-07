(ns ^:figwheel-no-load text-adventure-engine.app
  (:require [text-adventure-engine.core :as core]
            [devtools.core :as devtools]))

(enable-console-print!)

(devtools/install!)

(core/init!)
