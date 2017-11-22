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
						   (format t "You ring the doorbell")))))
	 :heartbeat-hooks nil
	 :enter-hooks nil
	 :exit-hooks nil)
   (list :name 'living-room
	 :description-fn (make-static-room-describer-fn "You are in the living room.")
	 :inventory nil
	 :actions nil
	 :heartbeat-hooks nil
	 :enter-hooks nil
	 :exit-hooks nil))
  )

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
