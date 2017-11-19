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




;; chapter 6; pg87
(defun say-hello ()
  (print "Please type your name: ")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))

;; (say-hello)
