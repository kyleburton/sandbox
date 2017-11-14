(load "mylib.lisp")
(defparameter *small* 1)
(defparameter *big* 100)


(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *big* (1+ (guess-my-number)))
  (guess-my-number))


(defun start-over ()
  (setf *small* 1)
  (setf *big*   100)
  (guess-my-number))

;; (/ 1 0)
;; unless
;; :thing
;; 'thing
;; (type-of :thing) KEYWORD
;; (type-of 'thing) SYMBOL
;; (type-of 123456) (INTEGER 0 281474976710655)
;; (type-of 123.45) SINGLE-FLOAT

;; pg 56

(defvar *arch-enemy* nil)

(defun pudding-eater (person)
  (cond
    ((eq person 'henry)
     (setf *arch-enemy* 'stupid-lisp-alien)
     '(curse you lisp alien - you ate my pudding))
    ((eq person 'johnny)
     (setf *arch-enemy* 'useless-old-johnny)
     '(i hope you choked on my pudding johnny))
    (t
     '(why you eat my pudding stranger ?))))

;; (pudding-eater 'johnny)
;; *arch-enemy* ;; USELESS-OLD-JOHNNY
;; (pudding-eater 'george-clooney)


(defun pudding-eater-2 (person)
  (case person
    ((henry)
     (setf *arch-enemy* 'stupid-lisp-alien)
     '(curse you lisp alien - you ate my pudding))
    ((johnny)
     (setf *arch-enemy* 'useless-old-johnny)
     '(i hope you choked on my pudding johnny))
    (otherwise
     '(why you eat my pudding stranger ?))))


;; (pudding-eater 'johnny)
;; *arch-enemy* ;; USELESS-OLD-JOHNNY
;; (pudding-eater 'george-clooney)

(defmacro comment (&body body)
  (declare (ignore body))
  nil)


(comment

  (member 1 '(3 4 1 5))
  ;; (1 5)

  (type-of (member 1 '(3 4 1 5)))
  ;; CONS

  (find-if #'oddp '(2 4 5 6))
  ;; 5

  (values 1 2 3)
  1
  2
  3

  (multiple-value-bind
	(a b c) (values 1 2 3)
    (format nil "a=~a b=~a c=~a" a b c))

  "a=1 b=2 c=3"

  (multiple-value-list (values 1 2 3))
  (1 2 3)

  (find-if #'null '(2 4 nil 6))
  NIL
  (multiple-value-list (find-if #'null '(2 4 nil 6)))
  (NIL)

  

  )


(defparameter *fruit* 'apple)

(cond
  ((eq *fruit* 'apple)
   'its-an-apple)
  ((eq *fruit* 'orange)
   'its-an-orang))
;; ITS-AN-APPLE




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
   rest
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

;; TODO: use place-item-at-location & create a function that initializes the world
