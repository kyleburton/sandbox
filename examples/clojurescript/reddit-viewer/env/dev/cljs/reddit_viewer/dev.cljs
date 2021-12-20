(ns ^:figwheel-no-load reddit-viewer.dev
  (:require
    [reddit-viewer.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
