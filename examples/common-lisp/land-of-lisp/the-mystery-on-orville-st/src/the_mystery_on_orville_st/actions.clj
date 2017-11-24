(ns the-mystery-on-orville-st.actions
  (:require
   [the-mystery-on-orville-st.data :as data]
   [clojure.string                 :as string]
   [schema.core                    :as s]
   [clojure.tools.logging          :as log]))

;; TODO: lock the front door, make the player unlock it and open it before they can walk through it
;; TODO: put the puzzle into the living room description, add a custom action to finish the puzzle.
;; TODO: finish adding edges for the house

(defonce actions (atom {}))

;; actions are functions that are passed (game-state player)
;; and can manipulate either the world state or the player's state
(def Action 
  {(s/required-key :name)        s/Keyword
   (s/required-key :description) [s/Str]
   (s/required-key :action-fn)    s/Any
   (s/required-key :aliases)     #{s/Str}})

(s/defn map->Action :- Action [m]
  m)

(s/defn describe-action :- s/Str [action :- Action]
  (format "%s (Try one of %s)"
          (string/join " " (:description action))
          (string/join "; " (map #(str "'" % "'") (:aliases action)))))

;; (command "help")

(s/defn register-action! :- s/Any [action :- Action]
  (swap! actions assoc (:name action) action))

(s/defn describe-edge :- s/Str [edge :- data/Edge]
  (let [destination (get (deref data/rooms) (-> edge :to))]
    (name (:name destination))))


(s/defn available-actions :- [Action] [game-state player :- data/Player]
  (let [room (data/player-location player)]
    (filter
     identity
     (concat
      (-> actions deref    vals)
      (-> room    :actions vals)
      (-> player  :actions vals)
      ;; TODO: include actions we can perform on or with our inventory
      ))))

(comment
  (the-mystery-on-orville-st.game/init!)
  (command "look")

  (available-actions
   data/game-state
   (-> data/players deref vals first))

  (let [room (data/player-location pp)]
    (filter
     identity
     (concat
      (-> actions deref    vals)
      (-> room    :actions vals)
      (-> pp      :actions vals)
      ;; TODO: include actions we can perform on or with our inventory
      )))


  
  (data/set-player-location! (-> data/players deref vals first) :front-porch)
  )

(s/defn action-matches-string? :- (s/maybe s/Bool) [action :- Action input :- s/Str]
  (some
   (fn [alias]
     (.startsWith (.toLowerCase input) alias))
   (:aliases action)))

(comment
  (action-matches-string?
   (map->Action {:name :testme :description ["the thing action"] :aliases #{"thing" "thing stuff"} :action-fn nil})
   "thing the stuff")
  (action-matches-string?
   (map->Action {:name :testme :description ["the thing action"] :aliases #{"thing" "thing stuff"} :action-fn nil})
   "do not do the thing")

  )

(s/defn strip-alias [action :- Action input :- s/Str]
  ;; find the longest matching alias
  ;; substring it off
  ;; trim the result
  (let [longest-alias (->>
                       (:aliases action)
                       (filter
                        (fn [alias]
                          (.startsWith (.toLowerCase input) alias)))
                       (sort-by #(.length %))
                       first)]
    (->
     input
     (.substring (count longest-alias))
     .trim)))

(s/defn do-action! [game-state :- data/GameState player :- data/Player & args]
  ;; args is the string the player typed in
  ;; find an action who's alias is the beginnign of the players command
  (let [[input & args]   args
        actions          (available-actions game-state player)
        matching-actions (filter #(action-matches-string? % input) actions)]
    (cond
      (= 1 (count matching-actions))
      (let [action    (-> matching-actions first)
            action-fn (:action-fn action)]
        (action-fn
         game-state
         player
         (strip-alias action input)))

      :otherwise
      (format "Er, I don't think you can do that here or I don't know what you mean.  Try 'help' or 'look'."))))

(s/defn look :- s/Str [game-state :- data/GameState player :- data/Player & args]
  (let [room              (data/player-location player)
        edges             (data/edges-from (:name room))
        items             (-> room :state deref :inventory)
        item-descriptions (if-not (empty? items)
                            (format "\n  You see: %s"
                                    (string/join ", " (->> items vals (map :name) (map name))))
                            "")]
    (format "%s\n\nFrom here you can go to any of: %s%s"
            (string/join
             "\n  "
             (:description room))
            (string/join "; "
                         (mapv describe-edge edges))
            item-descriptions)))

(comment
  (command "look")

  (let [player (-> data/players deref vals first)]
    #_(map describe-edge (data/edges-from (data/player-location player)))
    (data/player-location player))

  (look nil (-> data/players deref vals first))
  )

(s/defn help :- s/Str [game-state :- data/GameState player :- data/Player & args]
  (let [room (data/player-location player)]
    (format "Ok, here's what you can do right now:\n%s\n\n"
            (string/join "\n  "
                         (map describe-action (available-actions game-state player))))))

(s/defn walk :- s/Str [game-state :- data/GameState player :- data/Player & args]
  (let [dest      (-> args first name)
        dest-kw   (keyword dest)
        dest-room (-> data/rooms deref dest-kw)]
    (cond
      (not dest-room)
      (format "Sorry, you can't go to %s from here." dest)

      :otherwise
      (do
        (data/set-player-location! player dest-kw)
        (do-action! game-state player "look")))))

(comment
  (-> data/players deref vals first data/player-location)
  (look nil (-> data/players deref vals first))
  (walk nil (-> data/players deref vals first) :entry-way)
  (walk nil (-> data/players deref vals first) "entry-way")
  (walk nil (-> data/players deref vals first) :nowhere)
  (help nil (-> data/players deref vals first))

  xx

  (-> data/rooms deref :front-porch :actions vals first)
  
  (map
   describe-action
   (-> data/rooms deref :front-porch :actions))

  (the-mystery-on-orville-st.game/init!)

  (command "look")
  (command "drop keys")
  (command "go entry-way")
  (command "take all")
  (command "inventory")
  (command "drop all")
  )


(s/defn take-item :- s/Str [game-state :- data/GameState player :- data/Player & args]
  (let [item-descr (-> args first name)
        item-kw    (keyword item-descr)
        room       (data/player-location player)]
    (cond
      (and (= :all item-kw)
           (not (empty? (-> room :state deref :inventory))))
      (let [items (-> room :state deref :inventory vals)]
        (dosync
         (doseq [item items]
           (data/take-item-from-room! (:name item) (:name room))
           (data/put-item-in-player-inventory! (:name item) player)))
        (format "You picked up: %s"
                (string/join ", " (->> items (map :name) (map name)))))
      
      (not (data/item-is-in-room? item-kw (:name room)))
      (format "You can't pick up '%s' it doesn't seem to be here." item-descr)

      :otherwise
      (dosync
       (data/take-item-from-room! item-kw (:name room))
       (data/put-item-in-player-inventory! item-kw player)
       (format "You've picked up the %s" item-descr)))))

(s/defn drop-item :- s/Str [game-state :- data/GameState player :- data/Player & args]
  (let [item-descr (-> args first name)
        item-kw    (keyword item-descr)
        room       (data/player-location player)]
    (cond
      (and (= :all item-kw)
           (not (empty? (-> player :state deref :inventory))))
      (let [items (-> player :state deref :inventory vals)]
        (dosync
         (doseq [item items]
           (data/take-item-from-player-inventory! (:name item) player)
           (data/put-item-in-room! (:name item) (:name room))))
        (format "You dropped: %s"
                (string/join ", " (->> items (map :name) (map name)))))
      
      (not (data/player-has-item? player item-kw))
      (format "You can't drop '%s' you don't have it" item-descr)

      :otherwise
      (dosync
       (data/take-item-from-player-inventory! item-kw player)
       (data/put-item-in-room! item-kw (:name room))
       (format "You've dropped the %s" item-descr)))))


(s/defn command :- s/Str [s :- s/Str]
  (do-action!
   data/game-state
   (-> data/players deref vals first)
   s))

(comment
  (-> data/players deref vals first)
  (data/player-location (-> data/players deref vals first))

  (do
    (the-mystery-on-orville-st.game/init!)
    (data/set-player-location!
     (-> data/players deref vals first)
     :living-room)
    (data/put-item-in-player-inventory!
     :missing-puzzle-piece-1
     (-> data/players deref vals first))
    (data/put-item-in-player-inventory!
     :missing-puzzle-piece-2
     (-> data/players deref vals first)))
  
  (command "look")
  (command "help")
  (command "complete puzzle")
  (command "take keys")
  (command "inventory")

  (command "take keys")
  (command "drop keys")
  
  (command "help")
  (command "walk nowhere")
  (command "search mailbox")

  (command "walk entry-way")

  (command "walk front-porch")


  )


(s/defn init! []
  (register-action! (map->Action {:name        :help
                                  :aliases     #{"help"
                                                 "help me"
                                                 "what can i do"
                                                 "what can i do here"
                                                 "what should i do"}
                                  :description ["Get some help."]
                                  :action-fn   #'help}))
  (register-action! (map->Action {:name        :look
                                  :aliases     #{"look"
                                                 "take a look"
                                                 "look around"
                                                 "have a look"
                                                 "what can i see"
                                                 "what do i see"}
                                  :description ["Have a look at your surroundings."]
                                  :action-fn   #'look}))
  (register-action! (map->Action {:name        :walk
                                  :aliases     #{"walk"
                                                 "go"
                                                 "go to"
                                                 "goto"
                                                 "head to"}
                                  :description ["Walk to another area"]
                                  :action-fn   #'walk}))
  (register-action! (map->Action {:name        :take
                                  :aliases     #{"take"
                                                 "pickup"
                                                 "pick up"
                                                 "grab"}
                                  :description ["Pick up an item."]
                                  :action-fn   #'take-item}))
  (register-action! (map->Action {:name        :drop
                                  :aliases     #{"drop"
                                                 "discard"
                                                 "put down"}
                                  :description ["Drop an item."]
                                  :action-fn   #'drop-item})))

(comment

  (init!)

  )
