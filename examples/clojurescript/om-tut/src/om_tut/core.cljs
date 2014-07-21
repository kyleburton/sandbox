(ns om-tut.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state (atom {:list ["Lion" "Zebra" "Buffalo" "Antelope"]}))

(defn stripe [text bgc]
  (let [st #js {:backgroundColor bgc}]
    (dom/li #js {:style st} text)))

#_(om/root
  (fn [app owner]
    (apply dom/ul #js {:className "animals"}
      (map (fn [text] (dom/li nil text)) (:list app))))
  app-state
  {:target (. js/document (getElementById "app"))})

(om/root
  (fn [app owner]
    (om/component
      (apply dom/ul #js {:className "animals"}
        (map stripe (:list app) (cycle ["#ff0" "#fff"])))))
  app-state
  {:target (. js/document (getElementById "app"))})

#_(om/root
  (fn [app owner]
    (dom/h1 nil (:text app)))
  app-state
  {:target (. js/document (getElementById "app"))})

(comment



)
