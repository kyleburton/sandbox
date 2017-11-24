(ns the-mystery-on-orville-st.game
  (:require
   [the-mystery-on-orville-st.data :as data]
   [the-mystery-on-orville-st.actions :as actions]
   [clojure.tools.logging :as log]
   [clojure.data.json     :as json]))



(defn init! []
  (data/init!)
  (actions/init!))

(comment

  (init!)

  )
