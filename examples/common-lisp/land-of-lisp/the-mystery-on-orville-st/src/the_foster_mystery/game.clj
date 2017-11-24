(ns the-foster-mystery.game
  (:require
   [the-mystery-on-orville-st.data :as orville-data :refer [set-player-location! players]]
   [the-mystery-on-orville-st.actions :as actions :refer [command]]
   [the-mystery-on-orville-st.util :refer [render-to-graph]]
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
   :drive-way))


(comment
  
  ;; Remember:
  ;;   CTRL-x b             - switch to another buffer
  ;;   CTRL-c CTRL-k        - compile the current file
  ;;   CTRL-x CTRL-e        - run the code to the left of the cursor
  ;;   CTRL-x CTRL-s        - save your work
  ;;   CTRL-x o             - switch to the other window

  (do
    (init!)
    (render-to-graph @orville-data/rooms @orville-data/edges "foster.dot"))

  (command "look")
  (command "go another-room")
  (command "take all")
  (command "inventory")
  (command "drop lead-pipe")

  )
