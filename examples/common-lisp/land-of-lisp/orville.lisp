(defpackage :com.github.kyleburton.the-mystery-on-orville-st
  (:use :com.github.kyleburton.mylib
	:common-lisp)
  (:shadow :room))
(in-package :com.github.kyleburton.the-mystery-on-orville-st)

;; The Mystery on Orville St.
;;
;; This is a text adventure game made by Madison and Kyle Burton in
;; 2017.  The game follows a family who's moved from Pennsylvania to
;; Culver City CA, living on Orville St.


;; Data Structures
;;
;; The game consists of a graph of rooms connected by doors or hallways

;; there are global actions:
;;
;; * look
;; * take
;; 
;; room local actions (can only be performed when in that room)
;;
;; * living-room: turn-on-lights (in rooms that have light switches)
;; * living-room: turn-of-lights (in rooms that have light switches)
;;
;; item speicfic actions (only be performed when you're holding the item)
;;
;; * tv-remote: turn-on-tv
;; * tv-remote: turn-off-tv
;; * hdmi-cable: plug-in-cable
;;

(defstruct game-state
  name)

(defparameter *game-state* (make-game-state :name 'the-mystery-on-orville-st))

;; Rooms are the nodes in the graph
(defstruct room
  name             ;; symbol, must be unique
  description-fn   ;; creates user facing description
  inventory        ;; list of items in the room that can be manipulated or picked up
  actions          ;; assoc list of special actions that can be taken in the room
  heartbeat-hooks  ;; callbacks that can be executed every game tick
  enter-hooks      ;; callbacks executed whenever a player enters the room
  exit-hooks)      ;; callbacks executed whenever a player exits the room

(defparameter *rooms* nil) ;; assoc list of (name room)

(defstruct item
  name             ;; symbol, must be unique
  description-fn
  inventory
  heartbeat-hooks)

(defparameter *items* nil) ;; assoc list of (name item)

(defstruct player
  name            ;; symbol or string?  must be unique if we're going to support multiple simultaneous players
  description-fn  ;; do we need this?
  location        ;; the symbol name of the room the player is in
  inventory       ;; alist of items (name item) that the user is holding
  properties      ;; assoc list that represnts the user's state
  heartbeat-hooks)

(defparameter *players* nil)  ;; assoc list of (name player)

;; actions are functions that are passed (game-state player)
;; and can manipulate either the world state or the player's state
(defstruct action
  name          ;; unique symbol/keyword
  aliases       ;; the user may type any of these to invoke the handler
  handler-fn)   ;; callback

(defparameter *actions* nil)  ;; asoc list of (name action)



;; TODO: need a struct for our graph edges, ie: doorways / doors (so they can be opened & closed, locked etc)
;; 
;; (defstruct  
;;   name
;;   description-fn
;;   inventory
;;   heartbeat-hooks)

;; TODO: how are we going to deal with background threads in clisp?
;; TODO: what other entities should we have in the game?
;; * pets?  can they have behaviors?  internal state machines that update on global tick?
;; * inanimate objects like a clock

(defun make-static-room-describer-fn (msg)
  (lambda (world-state player-state room)
    (declare (ignore world-state player-state room))
    (format nil msg)))

(comment

  (funcall (make-static-room-describer-fn "this is a room") nil nil nil)
  )

;; TODO: move the rooms out to their own database
(comment

  )

(defun add-room! (room)
  (when (not (assoc (room-name room)
		    *rooms*))
    (setf *rooms* (cons (list (room-name room) room) *rooms*))))

(defparameter *hdmi-cable*
  (make-item
   :name 'hdmi-cable
   :description-fn (lambda (&rest args)
		     (declare (ignore args))
		     (format nil "6' hdmi cable"))
   :inventory nil
   :heartbeat-hooks nil))

(defparameter *all-rooms*
  (list
   (list :name 'front-porch
	 :description-fn (make-static-room-describer-fn "You are standing on the front porch of the house.  You can see the front door and a doorbell.")
	 :inventory nil
	 :actions (list
		   `(ring-doorbell
		     `(make-action :name 'ring-doorbell
				   :aliases '(ring-doorbell press-doorbell)
				   :handler-fn #'(lambda (game-state player)
						   (declare (ignore game-state player))
						   (format t "You ring the doorbell"))))))
   (list :name 'entry-way
	 :description-fn (make-static-room-describer-fn "You are in the entry-way."))
   (list :name 'front-room
	 :description-fn (make-static-room-describer-fn "You are in the front room.  This room has a large window through which you can see the sidewalk and Orville St.  There is a desk with a computer, keyboard, mouse and two monitors.  You see a piano, a couch, chair, some boxes and two bookshelves."))
   (list :name 'small-hallway
	 :description-fn (make-static-room-describer-fn "You are in a small hallway that connects the living room, the bathroom, Madison's room and Sydneys' room.  There is also a small closet who's door is ajar.  A strange, foul oder eminates from the closet ..."))
   (list :name 'madisons-room
	 :description-fn (make-static-room-describer-fn "You are standing in a bedroom.  There is a large bunkbed with an avalanche of stuffed animals covering the top bunk.  A beta fish swims lazily in a circular fish tank on a small table.  There are shelves of legos, baskets of legos, various objects built from legos, piles of loose legos, unopened lego kits ... you have a hunch that whomever lives in this room, they like legos.  There is a large bookshelf and a closet who's door will not shut because it is blocked with various things."))
   (list :name 'sydneys-room
	 :description-fn (make-static-room-describer-fn "You are standing in a bedroom.  There is a large raised bed with a ladder leading up to it.  Below the bed is a desk and some shelves.  There's a laptop computer on the desk where you can see a game of The Sims is in progress.  There is custom artowrk hung all around the room.  The desk has paintbrushes, charcoal pencils, colored pastel pencils and paints strewn across it.  There are several sketch pads and painting canvases here and there on the desk and in the shelves.  There is a closet who's door is closed.  There is another bed, that can convert between a bench and a bed - it's currently a bed and it's obvious from the disheveled nature that this is where our room's occupant sleeps.  You realize there is also a Guinnie Pig cage next to the door you entered from with Sydney's two Guinnie Pigs: Mr Nibbles and Gus-Gus."))
   (list :name 'dining-room
	 :description-fn (make-static-room-describer-fn "You are in the dining room.  There is a black rectangualr dining room table with four chairs around it.  There is a small bench with a basket of ... flashlights?  Yep, it's a basket of flashlights. *shrug* *me neither*  Next to the small bench rests Daisy's food and water bowls."))
   (list :name 'kitchen
	 :description-fn (make-static-room-describer-fn "This is the kitchen.  There is a long counter along the wall and a pretty substantial island that contains the range top, microwave oven and oven.  There are several drawers in both the main counter as well as the island.  There is a refridgerator that has a freezer, a sink and a dish rack.  There's a toaster, an electric kettle and a Ninja (tm) blender on the main counter.  There is a nice vase of flowers on at the end of the island nearest the trash can."))
   (list :name 'laundry-area
	 :description-fn (make-static-room-describer-fn "You're standing in the laundry nook next to the refridgerator.  There is an over/under washer and dryer here.  There's a small bathroom with a pocket door opposite the washer/dryer."))
   (list :name 'small-bathroom
	 :description-fn (make-static-room-describer-fn "You've managed to wedge yourself into an exteremely small bathroom.  You're not sure if you can turn around.  There is a sink tha'ts not much larger than a romance novel, a toilet, a trash can (that you relize you've got one foot in) and some shelves with spare rolls of toilet paper and boxes of tissues.  Under the sink are two small doors."))
   (list :name 'hamper-nook
	 :description-fn (make-static-room-describer-fn "You're in a small, squared off nook that contains a linen closet and a pull out hamper.  From here you can to go the master bedroom, Sydney's room or back into the living room."))
   (list :name 'master-bedroom
	 :description-fn (make-static-room-describer-fn "This is the master bedroom.  There are two dressers, one with a TV on it, a hamper, a coat rack.  There is a queen sized bed with a grey cat sleeping and purring quitely (his name is Oscar) on the bed.  On either side of the bed are night stands, each of which has a nightlight.  There is a nice walk-in closet to the left side of the bed.  There is a poster of three children on a beach, staring at a giant that seems to be formed from clouds on the horizon.  Its a nice picture.   There is a half-full laundy basket and piles of clean folded clothes on the bed."))
   (list :name 'back-patio
	 :description-fn (make-static-room-describer-fn "You stand on the concrete pad which forms the back patio.  There is a stainless steel propane grill, which has a cover on it.  There is a round table with four chairs around it and two additional Adirandack chairs.  There is a skateboard and some dog toys.   You can see the garage as well as the back yard from here."))
   (list :name 'back-yard
	 :description-fn (make-static-room-describer-fn "Overgrown is the first thing that come to mind as you survey the back yard.  There is a 6' cinderblock wall surrounding the yard.  You see a plum and an orange tree.  There are several oranges on the ground, one of which you notice is wiggling.  As you look more closely, two small eyes and a nose peek up at you and a rat scurry's away to the corner of the yard and escapes through a small crack in the wall to the apartment complex beyond.  There is a potting cart with a few empty pots and other gardening equipment."))
   (list :name 'front-of-garage
	 :description-fn (lambda (game-state player)
			   (declare (ignore game-state player))
			   (format t "There is a large, white garage door in front of you with a handle in the center.")))
   
   (list :name 'living-room
	 :description-fn (make-static-room-describer-fn "You are in the living room.  There's a black sideboard cabinet with the largest cork-screw device you've ever seen in your life on it, a desk with a computer, keyboard, mouse and monitor.  You see a worn green couch that looks like it's both seen better days and lots of love.  There's an oval wooden coffee table with several stacks of magazines on it. There is a flat-screen TV (is there any other kind these days?) on a half-height bookshelf next to a gas fireplace.  Strangely the gas fireplace has a bed of fractured bits of glass as balast, you wonder who came up with that idea.  There's a bookshelf lined with various board games - looking it over you recognize one of the games as Mechs Vs Minions from Riot Games.  There is a sliding glass door out to the back patio."))))

(defun add-rooms! ()
  (loop for room-info in *all-rooms*
     do
       (let ((room (apply #'make-room room-info)))
	 (add-room! room))))

;; *all-rooms*
;; *rooms*
;; (length *rooms*)
;; (add-rooms!)
;; (assoc 'living-roomx *rooms*)

(defun add-action! (action)
  (when (not (assoc (action-name action) *actions*))
    (setf *actions*
	  (cons
	   (list (action-name action) action)
	   *actions*))))

(defun add-actions! (actions)
  (loop for action-info in actions
     do
       (let ((action (apply #'make-action action-info))) 
	 (add-action! action))))

(comment

  (add-action!
   (apply #'make-action
	  `(:name look
		  :aliases (look see view look-around)
		  :handler-fn ,(lambda (game-state player)
				       (format nil  "TODO: describe the room state")))))

  *actions*

  )


(defun describe-inventory (inventory)
  (format nil
	  ;; "(~{~a~^, ~})"
	  "(~{~a~#[~;, and ~:;, ~]~})"
	  (mapcar
	   (lambda (item)
	     (funcall (item-description-fn item) item))
	   inventory)))

(comment
  
  (describe-inventory
   (list *hdmi-cable*
	 *hdmi-cable*
	 *hdmi-cable*))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global action functions, mostly for testing

(defun look ()
  ;; NB: only support 1 player for now
  (let* ((player   (first *players*))
	 (room     (cadr (assoc (player-location player) *rooms*))))
    (princ (funcall (room-description-fn room) *game-state* player room))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reset-game! ()
  (add-rooms!)
  (setf *players*
	(list
	 (make-player
	  :name 'player
	  :description-fn (lambda (game-state player)
			    (declare (ignore game-state player))
			    (format nil "You are ~a, currently holding: ~a~&"))
	  :location 'front-porch)))
  (add-action!
   (apply #'make-action
	  `(:name look
		  :aliases (look see view look-around)
		  :handler-fn ,(lambda (game-state player)
				       (declare (ignore game-state player))
				       (format nil  "TODO: describe the room state")))))

  ;; TODO: build a table of the aliases for actions so players can
  ;; call them via any alias
  )

(comment
  (reset-game!)

  )




(defun action! (action-sym)
  ;; either it's a global action, a room action, or an action from an item of the palyer's inventory
  (let* (;; (player         (first *players*))
	 (global-action  (assoc action-sym *actions*))

	 ;; (location       (cadr (assoc (player-location player) *rooms*)))
	 
	 ;; TODO: support localized (room) actions
	 ;; (room-actions    (assoc action-sym *actions*))

	 ;; TODO: support actions off of our inventory
	 ;; (inventory-actions ...)
	 )
    (cond
      ;; NB: only support one game-state & player
      (global-action
       (funcall (action-handler-fn (cadr global-action)) *game-state* (first *players*)))

      ;; (room-action
      ;;  (funcall (action-handler-fn (cadr action)) *game-state* (first *players*)))

      (t
       (format nil "Invalid action, sorry, you can't \"~a\" here :/" action-sym))))
  )

(comment

  (reset-game!)
  (look)

  (action! 'ring-doorbell)

  )
