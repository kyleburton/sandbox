(defpackage :com.github.kyleburton.land-of-lisp.game1
  (:use :common-lisp
	:com.github.kyleburton.mylib))
(in-package :com.github.kyleburton.land-of-lisp.game1)

(defparameter *nodes*
  '((living-room (you are in the living-room.  a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden. there is a well in front of you.))
    (attic (you are in the attic. there is a giant welding torch in the corner.))))


;; (assoc 'garden *nodes*)
;; => (GARDEN (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU.))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))


;; (describe-location 'living-room *nodes*)
;; => (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.)

;; TODO: allow aliases for the direction part of edges, use the first when printing the room description
(defparameter *edges*
  '((living-room
     (garden west door)
     (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;  (describe-path (-> 'living-room (assoc *edges*) cadr))  => (THERE IS A DOOR GOING WEST FROM HERE.)
;;  (describe-path (-> 'living-room (assoc *edges*) caddr)) => (THERE IS A LADDER GOING UPSTAIRS FROM HERE.)


(defun describe-paths (location edges)
  (->>
   (assoc location edges)
   cdr
   (mapcar #'describe-path)
   (apply #'append)))

;; (describe-paths 'living-room *edges*)

;; (mapcar #'sqrt (range 1 6))
;; => (1 1.4142135 1.7320508 2 2.236068)

(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *object-locations*
  '((whiskey living-room)
    (bucket living-room)
    (frog garden)
    (chain garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))


;; (objects-at 'living-room *objects* *object-locations*)


(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply
     #'append
     (mapcar
      #'describe-obj
      (objects-at loc objs obj-loc)))))

;; (describe-objects 'living-room *objects* *object-locations*)
;; => (YOU SEE A WHISKEY ON THE FLOOR. YOU SEE A BUCKET ON THE FLOOR.)


(defparameter *location* 'living-room)

(defun look ()
  (append
   (describe-location *location* *nodes*)
   (describe-paths    *location* *edges*)
   (describe-objects  *location* *objects* *object-locations*)))

;; (look)

;; (cdr (assoc 'living-room *edges*))

(defun walk (direction)
  ;; aka "find location for direction"
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn
	  (setf *location* (car next))
	  (look))
	'(you cannot go that way))))

;; (look)
;; (walk 'west)
;; (look)
;; (walk 'east)

(defun remove-item-from-location (item location)
  (setf *object-locations*
	(remove-if #'(lambda (elt)
		       (equalp elt (list item location)))
		   *object-locations*)))

(defun place-item-at-location (item location)
  (push (list item location) *object-locations*))

(defun item-at-location-p (item location)
  (member item
	  (objects-at location *objects* *object-locations*)))

(defun pickup (object)
  (cond
    ((or (equalp object 'all)
	 (equalp object 'everything))
     (loop for object in (objects-at *location* *objects* *object-locations*)
	do
	  (remove-item-from-location object *location*)
	  (place-item-at-location    object 'body))
     `(you are now carrying ,@(inventory)))
    ((item-at-location-p object *location*)
     (remove-item-from-location object *location*)
     (place-item-at-location object 'body)
     `(you are now carrying the ,object))
    (t
     '(you cannot get that.))))

;; (look)
;; (inventory)
;; (pickup 'all)
;; (drop 'whiskey)

;; (look)
;; (pickup 'whiskey)
;; (look)
;; (pickup 'bucket)
;; (look)

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; (inventory)

;; TODO: I want to implement a drop command: needs to remove all items from 'body
;; and place them in *location*


;; *object-locations*
(defun drop (object)
  (cond
    ((equalp object 'all)
     (loop for item in (objects-at 'body *objects* *object-locations*)
	do
	  (format t "drop: object=~a~&" item)
	  (drop item)))
    ((item-at-location-p object 'body)
     (remove-item-from-location object 'body)
     (place-item-at-location object *location*)
     (inventory))
    (t
     '(you cannot drop that.))))

;; (inventory)
;; (drop 'whiskey)


'(
  (look)
  (pickup 'all)
  (inventory)

  (drop 'all)
  
  (pickup 'whiskey)
  (pickup 'bucket)
  (inventory)
  (walk 'west)
  (drop 'whiskey)
  (drop 'bucket)
  (look)
  (walk 'east)
  (look)
  (inventory)
  (walk 'west)
  (pickup 'all)

  )

(defparameter *allowed-commands*
  '(look
    walk
    pickup
    inventory))

(defparameter *command-aliases*
  '((go   walk)
    (head walk)
    (get  pickup)
    (take pickup)
    (l    look)))

(defun resolve-alias (cmd)
  (let ((entry (find cmd *command-aliases* :key #'car))) 
    (cond
      (entry (cadr entry))
      (t cmd))))

;; (find 'go *command-aliases* :key #'car)
;; (resolve-alias 'pickup)

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (resolve-alias (car cmd)) (mapcar #'quote-it (cdr cmd))))))

;; TODO: other considerations for commands: some rooms or some items
;; may have commands that are only applicable to them specifically
;; TODO: if we do support multiple-players, each player will need an inventory ...
;; TODO: use place-item-at-location & create code to initializes the world to it's start state

(defun game-repl ()
  (labels ((inner-repl ()
	   (let ((cmd (game-read)))
	     (unless (eq (car cmd) 'quit)
	       (game-print (game-eval cmd))
	       (inner-repl)))))
    (game-print (look))
    (inner-repl)))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      `(i do not know the command ,(format nil "~a" (car sexp)) - please try one of ,@*allowed-commands*)))

;; (game-eval '(thing that stuff))
;; (game-eval '(look))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond
	((eql item #\space)
	 (cons item (tweak-text rest caps lit)))
	((member item '(#\! #\? #\.))
	 (cons item (tweak-text rest t lit)))
	(lit
	 (cons item (tweak-text rest nil lit)))
	(caps
	 (cons (char-upcase item) (tweak-text rest nil lit)))
	(t
	 (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ
   (coerce (tweak-text (coerce (string-trim "()" (prin1-to-string lst))
			       'list)
		       t
		       nil)
	   'string))
  (fresh-line))


;; (game-print '(not only does this sentence have a "comma," it also mentions the "iPad."))


;; (nodes->dot *nodes*)
