(ns the-foster-mystery.data
  (:require
   [the-mystery-on-orville-st.data :refer [game-state rooms edges items players
                                           Item Room Player Edge GameState
                                           map->Item
                                           map->Room
                                           map->Player
                                           map->Edge
                                           player-inventory
                                           put-item-in-room!]]
   [schema.core :as s]
   [clojure.string :as string]
   [clojure.tools.logging :as log]))

(declare search-room)

(def room-data
  [{:name :some-room
    :description ["This is some room."
                  "Sydney has to describe it here."]
    :actions     {:search {:name        :search
                           :aliases     #{"search" "investigate"}
                           :description ["Investigate something more closely."]
                           :action-fn   #'search-room}}}
   {:name :another-room
    :description ["This is the second room."]}
   
   ;; more rooms go here
   ])

(s/defn search-room [game-state :- GameState player :- Player & args]
  (format "TODO: implement this search! args=%s" args))

(def player-data [{:name            "You, yourself."
                   :description     ["You look like ... yourself."]
                   :heartbeat-hooks []
                   :actions         {:inventory {:name        :inventory
                                                 :aliases     #{"inventory" "what have i got" "what do i have"}
                                                 :description ["Take stock of what you're carrying"]
                                                 :action-fn   #'player-inventory}}
                   :state           (atom {:location :front-porch
                                           :inventory {}})}])

(def edge-data
  (conj
   (reduce
    (fn [acc [from to]]
      (conj acc
            {:from from :to to}
            {:from to   :to from}))
    []
    [[:some-room :another-room]])))


(def item-data [{:name        :lead-pipe
                 :description ["This is a lead pipe."
                               "It's very heavy and would make a good weapon ... if one were so inclined."]}])

(defn init! []
  (reset! rooms
          (->>
           room-data
           (map map->Room)
           (reduce (fn [acc ent] (assoc acc (:name ent) ent)) {})))
  (reset! items
          (->>
           item-data
           (map map->Item)
           (reduce (fn [acc ent] (assoc acc (:name ent) ent)) {})))
  (reset! players
          (->>
           player-data
           (map map->Player)
           (reduce (fn [acc ent] (assoc acc (:name ent) ent)) {})))
  (reset! edges
          (->>
           edge-data
           (map map->Edge)))
  (put-item-in-room! :lead-pipe :another-room))
