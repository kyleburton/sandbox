(defpackage :com.github.kyleburton.the-mystery-on-orville-st
  (:use :com.github.kyleburton.mylib
	:common-lisp))
(in-package :com.github.kyleburton.the-mystery-on-orville-st)

;; The Mystery on Orville St.
;;
;; This is a text adventure game made by Madison and Kyle Burton in
;; 2017.  The game follows a family who's moved from Pennsylvania to
;; Culver City CA, living on Orville St.


;; Data Structures
;;
;; The game consists of a graph of rooms connected by doors or hallways


;; Rooms are the nodes in the graph
(defstruct room
  name             ;; symbol, must be unique
  description-fn   ;; creates user facing description
  inventory        ;; list of items in the room that can be manipulated or picked up
  actions          ;; assoc list of special actions that can be taken in the room
  heartbeat-hooks  ;; callbacks that can be executed every game tick
  enter-hooks      ;; callbacks executed whenever a player enters the room
  exit-hooks)      ;; callbacks executed whenever a player exits the room

'(defstruct item
  name             ;; symbol, must be unique
  description-fn
  inventory
  heartbeat-hooks)

'(defstruct player
  name
  description-fn
  inventory
  properties      ;; assoc list that represnts the user's state
  heartbeat-hooks)

;; TODO: need a struct for doorways / doors (so they can be opened & closed
;; 
;; (defstruct  
;;   name
;;   description-fn
;;   inventory
;;   heartbeat-hooks)

;; TODO: what other entities should we have in the game?
;; * the pets?
;; * inanimate objects like a clock


;; TODO: move the rooms out to their own database
;; (make-room
;;  :name 'living-room
;;  description-fn )


(defparameter *global-player-actions* nil)

(defstruct player-action
  name
  handler-fn
  state)

(comment

  (let ((thing 3))
    (format t "thing:~a~&" thing)
    (setf thing 4)
    (format t "thing:~a~&" thing))

  )


(defun register-action! (name handler-fn state)
  '(setf *global-player-actions*
    (cons
     (make-player-action
      :name name
      :handler-fn handler-fn
      :state state)
     *global-player-actions*)))
