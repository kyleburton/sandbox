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
  [{:name :drive-way
    :description ["You look and see the house. It is a two story building with double doors in frount."
                  "To the side of the doors,is a mail box."]}
   {:name :living-room
    :description ["As you look around, you see a fire place with a nicly lit fire glowing in it. In frount of the fire place is a sitting area with a table and chairs. In this room there are lots of paintings."]
    :actions     {:search {:name        :search
                           :aliases     #{"search" "investigate"}
                           :description ["Investigate something more closely."]
                           :action-fn   #'search-room}}}
   
   ;; more rooms go here
   {:name :dining-room
    :description ["In this room, there is a table with five chairs around it. In the corner there is a grandfather clock."
                  "The clock says the time is 7 o'clock."]}
   {:name :library
    :description ["In the library is alot of shelvs covered in books. the is a bay window along one wall"
                  "where Lily, Will's wife, sits."]}
   {:name :study
    :description ["In the study, there is a desk witha chair.There are a few shelvs covered in verious things."
                  "In one corner there is a globe. In another corner lies Will's bloody body."
                  ]}
   {:name :bedroom
    :description ["In the bedroom is a neatly made bed. on either side of the bed are tables with lamps."
                  "Ohter than that the room is mostly empty exept for a box."
                  ]}
   {:name :kitchen
    :description ["The Kitchen looks like anyother kitchen. It has stoves and ovens. A sink. The Cook,"
                  "Polly, is washing up the pots and pans."
                  ]}
   {:name :upstairs-hall
    :description ["The hall is conected to three rooms. Agenst one wall is a table with a plant."
                  ]}
   {:name :master-bedroom
    :description ["This room has a big bed. There are two dressers. One dresser has books and jewelry"
                  "on top. The other has nothing on top."
                  ]}
   {:name :guest-bedroom
    :description ["This beedroom has a bed and a dresser. On the bed is Aunt June."
                  ]}
   {:name :left-bedroom
    :description ["In this bedroom, the maid, Sarah, is making the bed."
                  ]}
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
    [[:drive-way :living-room]
     [:living-room :dining-room]
     [:living-room :study]
     [:living-room :library]
     [:living-room :bedroom]
     [:living-room :kitchen]
     [:living-room :upstairs-hall]
     [:upstairs-hall :master-bedroom]
     [:upstairs-hall :guest-bedroom]
     [:upstairs-hall :left-bedroom]
     [:dining-room :kitchen]])))


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
  ;; (put-item-in-room! :lead-pipe :another-room)
  )


;; TODO: code up our dog, Daisy, lets use a background thread and a state machine :-D
;; what do we 
(comment
  ;; what behavior do we
  {:name :daisy
   :start-state :sitting
   :states {:sitting {}
            }}

)

