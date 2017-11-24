(ns the-foster-mystery.game
  (:require
   [the-mystery-on-orville-st.data :as orville-data :refer [set-player-location! players]]
   [the-mystery-on-orville-st.actions :as actions :refer [command]]
   [the-foster-mystery.data :as foster-data]
   [the-foster-mystery.actions :as foster-actions]
   [clojure.tools.logging :as log]
   [clojure.data.json     :as json]))


(defn init! []
  (orville-data/init-players!)
  (foster-data/init!)
  (foster-actions/init!)
  (set-player-location!
   (-> players deref vals first)
   :some-room))


(comment
  
  ;; Remember:
  ;;   CTRL-x b             - switch to another buffer
  ;;   CTRL-c CTRL-k        - compile the current file
  ;;   CTRL-x CTRL-e        - run the code to the left of the cursor
  ;;   CTRL-x CTRL-s        - save your work

  (log/infof "hi: %s" (java.util.Date.))

  (init!)

  (command "look")
  (command "go another-room")
  (command "take all")
  (command "inventory")
  (command "drop lead-pipe")

  )
