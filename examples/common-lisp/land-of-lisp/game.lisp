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

