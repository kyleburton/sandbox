(ns the-mystery-on-orville-st.data
  (:require
   [schema.core :as s]
   [clojure.string :as string]
   [clojure.tools.logging :as log]))

(def GameState clojure.lang.Atom)
(defonce game-state (atom {}))
(defonce rooms (atom {}))
(defonce edges (atom []))
(defonce items (atom {}))
(defonce players (atom {}))

(def Item
  {(s/required-key :name)            s/Keyword
   (s/required-key :description)     [s/Str]
   (s/required-key :description-fn)  s/Any
   (s/required-key :heartbeat-hooks) [s/Any]
   (s/required-key :state)           s/Any})

(def Inventory {s/Keyword Item})

(def Room
  {(s/required-key :name)            s/Keyword      ;; symbol, must be unique
   (s/required-key :description)     [s/Str] ;; if description-fn is nil, fall back to the default description fn using this string
   (s/required-key :description-fn)  s/Any          ;; creates user facing description
   (s/required-key :actions)         s/Any          ;; assoc list of special actions that can be taken in the room
   (s/required-key :heartbeat-hooks) s/Any          ;; callbacks that can be executed every game tick
   (s/required-key :enter-hooks)     s/Any          ;; callbacks executed whenever a player enters the room
   (s/required-key :exit-hooks)      s/Any          ;; callbacks executed whenever a player exits the room
   (s/required-key :state)           clojure.lang.Atom})


(def Player
  {(s/required-key :name)            (s/either s/Str s/Keyword) ;; symbol or string?  must be unique if we're going to support multiple simultaneous players 
   (s/required-key :actions)         {s/Keyword s/Any} ;; TODO: should be an Action
   (s/required-key :description)     [s/Str] ;; if description-fn is nil, fall back to the default description fn using this string 
   (s/required-key :heartbeat-hooks) [s/Any]
   (s/required-key :state)           clojure.lang.Atom})


(def Edge
  {(s/required-key :from)        s/Any
   (s/required-key :to)          s/Any
   (s/required-key :description) [s/Any]
   (s/required-key :state)       s/Any
   (s/required-key :follow-fn)   s/Any})


(declare complete-puzzle)
(declare search-toolbox)

;; TODO: move all the data out of the code
(def room-data
  [{:name        :front-porch
    :description ["You are standing on the front porch of the house."
                  "You can see the front door and a doorbell."],
    :actions     {:ring-doorbell {:name        :ring-doorbell
                                  :aliases     #{"ring" "ring doorbell" "ring bell"}
                                  :description ["Ring the doorbell"]
                                  :action-fn   (fn [game-state player & args] (format "You ring the doorbell"))}
                  :search-mailbox {:name        :search-mailbox
                                   :aliases     #{"open mailbox" "search mailbox"}
                                   :description ["Search the mailbox."]
                                   :action-fn   (fn [game-state player & args] (format "You find mail for several people who don't live here.  You leave it in the mailbox."))}}}
   
   {:name        :entry-way
    :description ["You are in the entry-way."]}
   
   {:name        :front-room
    :description ["You are in the front room."
                  "This room has a large window through which you can see the sidewalk and Orville St."
                  "There is a desk with a computer, keyboard, mouse and two monitors."
                  "You see a piano, a couch, chair, some boxes and two bookshelves."]}
   
   {:name        :small-hallway
    :description ["You are in a small hallway that connects the living room, the bathroom, Madison's room and Sydneys' room."
                  "There is also a small closet who's door is ajar."
                  "A strange, foul oder eminates from the closet ..."]}
   
   {:name        :madisons-room
    :description ["You are standing in a bedroom."
                  "There is a large bunkbed with an avalanche of stuffed animals covering the top bunk."
                  "A beta fish swims lazily in a circular fish tank on a small table."
                  "There are shelves of legos, baskets of legos, various objects built from legos, piles of loose legos, "
                  "unopened lego kits ... you have a hunch that whomever lives in this room, they like legos."
                  "There is a large bookshelf and a closet who's door will not shut because it is blocked with various things."]}
   
   {:name        :sydneys-room
    :description ["You are standing in a bedroom."
                  "There is a large raised bed with a ladder leading up to it."
                  "Below the bed is a desk and some shelves."
                  "There's a laptop computer on the desk where you can see a game of The Sims is in progress."
                  "There is custom artowrk hung all around the room"
                  "The desk has paintbrushes, charcoal pencils, colored pastel pencils and paints strewn across it"
                  "There are several sketch pads and painting canvases here and there on the desk and in the shelves"
                  "There is a closet who's door is closed"
                  "There is another bed, that can convert between a bench and a bed - it's currently a bed and it's obvious from the disheveled nature that this is where our room's occupant sleeps"
                  "You realize there is also a Guinnie Pig cage next to the door you entered from with Sydney's two Guinnie Pigs: Mr Nibbles and Gus-Gus."]}
   
   {:name        :dining-room
    :description ["You are in the dining room"
                  "There is a black rectangualr dining room table with four chairs around it"
                  "There is a small bench with a basket of ... flashlights?  Yep, it's a basket of flashlights. *shrug* *me neither*  Next to the small bench rests Daisy's food and water bowls."]}
   
   {:name        :kitchen
    :description ["This is the kitchen"
                  "There is a long counter along the wall and a pretty substantial island that contains the range top, microwave oven and oven"
                  "There are several drawers in both the main counter as well as the island"
                  "There is a refridgerator that has a freezer, a sink and a dish rack"
                  "There's a toaster, an electric kettle and a Ninja (tm) blender on the main counter"
                  "There is a nice vase of flowers on at the end of the island nearest the trash can."]}
   
   {:name        :laundry-area
    :description ["You're standing in the laundry nook next to the refridgerator"
                  "There is an over/under washer and dryer here"
                  "There's a small bathroom with a pocket door opposite the washer/dryer."]}
   
   {:name        :small-bathroom
    :description ["You've managed to wedge yourself into an exteremely small bathroom"
                  "You're not sure if you can turn around"
                  "There is a sink tha'ts not much larger than a romance novel, a toilet, a trash can (that you relize you've got one foot in) and some shelves with spare rolls of toilet paper and boxes of tissues"
                  "Under the sink are two small doors."]}

   {:name        :main-bathroom
    :description ["This is the main bathroom, there are two sinks and a shower."
                  "There are some drawers beneath the sinks and a small closet."
                  "A shower curtain is pushed to one side in front of the tub and there are various bottles of shampoo, conditioner and soap on a small shelf."]}
   
   {:name        :hamper-nook
    :description ["You're in a small, squared off nook that contains a linen closet and a pull out hamper."
                  "From here you can to go the master bedroom, Sydney's room or back into the living room."]}
   
   {:name        :master-bedroom
    :description ["This is the master bedroom"
                  "There are two dressers, one with a TV on it, a hamper, a coat rack"
                  "There is a queen sized bed with a grey cat sleeping and purring quitely (his name is Oscar) on the bed"
                  "On either side of the bed are night stands, each of which has a nightlight"
                  "There is a nice walk-in closet to the left side of the bed"
                  "There is a poster of three children on a beach, staring at a giant that seems to be formed from clouds on the horizon"
                  "Its a nice picture"
                  " There is a half-full laundy basket and piles of clean folded clothes on the bed."]}
   
   {:name        :back-patio
    :description ["You stand on the concrete pad which forms the back patio"
                  "There is a stainless steel propane grill, which has a cover on it"
                  "There is a round table with four chairs around it and two additional Adirandack chairs"
                  "There is a skateboard and some dog toys"
                  " You can see the garage as well as the back yard from here."]}
   
   {:name        :back-yard
    :description ["Overgrown is the first thing that come to mind as you survey the back yard"
                  "There is a 6' cinderblock wall surrounding the yard"
                  "You see a plum and an orange tree"
                  "There are several oranges on the ground, one of which you notice is wiggling"
                  "As you look more closely, two small eyes and a nose peek up at you and a rat scurry's away to the corner of the yard and escapes through a small crack in the wall to the apartment complex beyond"
                  "There is a potting cart with a few empty pots and other gardening equipment."]}
   
   {:name        :front-of-garage
    :description ["There is a large, white garage door in front of you with a handle in the center."]}

   {:name        :hallway-closet
    :description ["You don't really want be in here, it smells bad.  There's a cat litter box sitting on the floor and there isn't really anywhere for your to stand."]}

   {:name        :master-bedroom-closet
    :description ["This is a pretty nice walk-in closet.  There are women's and men's clothes hung on hangers and a few sweaters and hats on shelves.  There are _lots_ of shoes here.  You also see a small stepping stool."]
    :actions     {:name        :sit-on-stool
                  :aliases     #{"sit" "sit down" "sit on the stool" "sit down on the stool"}
                  :description ["Sit on the stool."]
                  :action-fn   (fn [game-state player]
                                 (string/join "\n   "
                                              ["Sitting on the stool you realize it's pretty low to the ground and isn't all that comforable."
                                               "After thinking to yoursel for a moment, you stand back up and decide to keep searching the house."]))}}
   
   {:name        :living-room
    :description ["You are in the living room"
                  "There's a black sideboard cabinet with the largest cork-screw device you've ever seen in your life on it, a desk with a computer, keyboard, mouse and monitor."
                  "You see a worn green couch that looks like it's both seen better days and lots of love."
                  "There's an oval wooden coffee table with several stacks of magazines on a lower shelf."
                  "On the surface of the coffee table is a picture puzzle of a beautiful family of four, with a happy looking dog and two cats."
                  "There are two piece missing from the puzzle, you wonder where they might be."
                  "There is a flat-screen TV (is there any other kind these days?) on a half-height bookshelf next to a gas fireplace."
                  "Strangely the gas fireplace has a bed of fractured bits of glass as balast, you wonder who came up with that idea."
                  "There's a bookshelf lined with various board games - looking it over you recognize one of the games as Mechs Vs Minions from Riot Games."
                  "There is a sliding glass door out to the back patio."]
    :actions     {:complete-puzzle {:name        :complete-puzzle
                                    :aliases     #{"complete puzzle" "finish puzzle" "assemble puzzle"}
                                    :description ["Finish the puzzle"]
                                    :action-fn   #'complete-puzzle}}}
   {:name        :garage-interior
    :description ["Surveying the interior of the garage, you see lots of boxes, toys, bikes, a skateboard and four spare tires for a car (not sure why those are in there)."
                  "There is a large standing toolbox on one side in the back, behind the bicyles."]
    :actions     {:search-toolbox {:name        :search-toolbox
                                   :aliases     #{"search toolbox"}
                                   :description ["Search the toolbox"]
                                   :action-fn   #'search-toolbox}}}])

(s/defn map->Room :- Room [m]
  (merge
   {:description-fn  nil
    :actions         #{}
    :heartbeat-hooks []
    :enter-hooks     []
    :exit-hooks      []
    :state           (atom {:inventory {}})}
   m))

(s/defn rand-elt [elts]
  (nth elts (rand-int (count elts))))

(s/defn player-inventory :- s/Str [game-state :- GameState player :- Player & args]
  (let [inventory (-> player :state deref :inventory)]
    (cond
      (empty? inventory)
      (rand-elt ["You have nothing." "You don't seem to be carrying anything." "You're empty handed." "You're not carrying anything, why not pick something up?"])

      :otherwise
      (format "You're carrying: %s"
              (string/join ", "
                           (->>
                            inventory
                            vals
                            (map :name)
                            (map name)))))))

(def player-data [{:name            "You, yourself."
                   :description     ["You look like ... yourself."]
                   :heartbeat-hooks []
                   :actions         {:inventory {:name        :inventory
                                                 :aliases     #{"inventory" "what have i got" "what do i have"}
                                                 :description ["Take stock of what you're carrying"]
                                                 :action-fn   #'player-inventory}}
                   :state           (atom {:location :front-porch
                                           :inventory {}})}])

(s/defn map->Player :- Player [m]
  (merge
   {:heartbeat-hooks []
    :actions {}
    :state   (atom {:location :front-porch
                    :inventory {}})}
   m))

(def item-data [{:name        :keys
                 :description ["Your keyring, which, strangely, has a small plastic skull attached by a short chain."
                               "On the keyring there's a key to the front-door, the garage, the mailbox and one you can't remember what it's to."]}
                {:name        :missing-puzzle-piece-1
                 :description ["A puzzle piece that's mostly red."]}
                {:name        :missing-puzzle-piece-2
                 :description ["A puzzle piece that's mostly blue."]}])

(s/defn map->Item :- Item [m]
  (merge
   {:description-fn  nil
    :heartbeat-hooks []
    :state           (atom {:inventory {}})}
   m))

(def edge-data
  (conj
   (reduce
    (fn [acc [from to]]
      (conj acc
            {:from from :to to}
            {:from to   :to from}))
    []
    [[:entry-way       :front-room]
     [:entry-way       :dining-room]
     [:dining-room     :kitchen]
     [:kitchen         :laundry-area]
     [:kitchen         :living-room]
     [:living-room     :small-hallway]
     [:living-room     :hamper-nook]
     [:hamper-nook     :sydneys-room]
     [:hamper-nook     :master-bedroom]
     [:small-hallway   :front-room]
     [:small-hallway   :madisons-room]
     [:small-hallway   :sydneys-room]
     [:living-room     :back-patio]
     [:back-patio      :front-of-garage]
     [:back-patio      :back-yard]
     [:main-bathroom   :small-hallway]
     [:main-bathroom   :living-room]
     [:laundry-area    :small-bathroom]
     [:small-hallway   :hallway-closet]
     [:master-bedroom  :master-bedroom-closet]
     [:front-of-garage :garage-interior]])
   {:from      :front-porch
    :to        :entry-way
    :follow-fn (fn [game-state player]
                 (format "TODO: you can only go through here if you unlock + open the door")
                 false)}
   {:from      :entry-way
    :to        :front-porch
    :follow-fn (fn [game-state player]
                 (format "TODO: you can only go through here if you unlock + open the door")
                 false)}))


(s/defn map->Edge :- Edge [m]
  (merge
   {:state       (atom {:inventory {}})
    :description ["Strange, this edge has no description"]
    :follow-fn   nil}
   m))

(s/defn edges-from :- [Edge] [room-name :- s/Keyword]
  (filter #(= room-name (:from %))
          @edges))

(s/defn edges-to :- [Edge] [room-name :- s/Keyword]
  (filter #(= room-name (:to %))
          @edges))

(s/defn player-location :- Room [player :- Player]
  (get @rooms (-> player :state deref :location)))

(s/defn set-player-location! [player :- Player location :- s/Keyword]
  (swap! (-> player :state)
         assoc :location location))

(s/defn item-is-in-room? :- (s/maybe s/Bool) [item :- s/Keyword location :- s/Keyword]
  (contains?
   (-> rooms deref location :state deref :inventory)
   item))

(s/defn player-has-item? :- (s/maybe s/Bool) [player :- Player item :- s/Keyword]
  (contains?
   (-> player :state deref :inventory)
   item))

(comment
  (item-is-in-room? :keys :entry-way)
  )

(s/defn put-item-in-room! [item-kw :- s/Keyword location :- s/Keyword]
  (let [room (-> rooms deref location)
        item (-> items deref item-kw)]
    (cond
      (not room)
      (throw (RuntimeException. (format "Sorry, you can't put item:%s into room:%s, room doens't exist!"
                                        item-kw (:name location))))

      (not item)
      (throw (RuntimeException. (format "Sorry, you can't put item:%s into room:%s, the item doens't exist!"
                                        item-kw (:name location))))
      
      :otherwise
      (swap!
       (:state room)
       #(update-in
         %
         [:inventory]
         assoc (:name item) item)))))

(s/defn take-item-from-room! [item :- s/Keyword location :- s/Keyword]
  (let [room (-> rooms deref location)]
    (swap!
     (:state room)
     #(update-in
       %
       [:inventory]
       dissoc item))))

(s/defn put-item-in-player-inventory! [item :- s/Keyword player :- Player]
  (swap!
   (:state player)
   #(update-in
     %
     [:inventory]
     assoc item (-> items deref item))))

(s/defn take-item-from-player-inventory! [item :- s/Keyword player :- Player]
  (swap!
   (:state player)
   #(update-in
     %
     [:inventory]
     dissoc item)))

(comment
  (-> players deref vals first :state deref :inventory keys)

  (put-item-in-player-inventory!
   :keys
   (-> players deref vals first))

  (let [player (-> players deref vals first)]
    (swap!
     (:state player)
     #(update-in
       %
       [:inventory]
       dissoc nil)))

  )


(defn complete-puzzle [game-state player & args]
  (let [has-all-pieces? (and (player-has-item? player :missing-puzzle-piece-1)
                             (player-has-item? player :missing-puzzle-piece-2))]
    (cond
      has-all-pieces?
      (string/join "\n  "
                   ["You assemble the final two pieces of the puzzle and admire the completed work."
                    "Congraulations you've solved the mystery at Orville St!"])
      :otherwise
      (string/join "\n  "
                   ["Sorry, you don't seem to have all the pieces.  Try searching around the house."]))))

(defn search-toolbox [game-state player & args]
  (string/join "\n  "
               ["You rummage through the toolbox though there don't seem to be any puzzle pieces here."]))


(defn init-players! []
  (reset! players
          (->>
           player-data
           (map map->Player)
           (reduce (fn [acc ent] (assoc acc (:name ent) ent)) {}))))

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
  (init-players!)
  (reset! edges
          (->>
           edge-data
           (map map->Edge)))
  (put-item-in-room! :keys :entry-way)
  (put-item-in-room! :missing-puzzle-piece-1 :madisons-room)
  (put-item-in-room! :missing-puzzle-piece-2 :sydneys-room))


(comment
  (init!)

  (player-location (-> players deref vals first))

  (-> players deref vals first :state)
  (-> players deref vals first :state deref :location)
  (-> players deref vals first player-location)

  (-> players deref vals first (set-player-location! :front-room))

  )
