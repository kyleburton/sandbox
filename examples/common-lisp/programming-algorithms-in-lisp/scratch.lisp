;; '(ql:system-apropos "vecto")
;; '(ql:quickload "system-name")
;; '(ql:add-to-init-file)
(defpackage :scratch
  ;; (:import-from :ppcre :regex-replace)
  (:use :cl))
(in-package :scratch)

(load "./.quicklisp/setup.lisp")

(ql:quickload "rutils")
(ql:quickload "named-readtables")
(ql:quickload :quicklisp-slime-helper)
(named-readtables:in-readtable rutils:rutils-readtable)

(defstruct (pair (:type list) (:conc-name nil))
  "A generic pair with left (LT) and right (RT) elements"
  lt rt)


(defun pair (x y)
  (make-pair :lt x :rt y))


(defun scratch ()
  (princ "hashtable? ")
  (princ #{:a 1 :b 2})

  (print (pair 1 2)))


;; (scratch::scratch)


(defstruct point
  parent) ;; if the parent is null the point is the root

(defun uf-union (point1 point2)
  "Join the subset of POINT1 and POINT2."
  (setf (point-parent point1) (or (point-parent point2)
                                  point2)))


(defun uf-find (point)
  "Determine the id of the subset that a POINT belongs to."
  (let ((parent (point-parent point)))
    (if parent
        (uf-find parent)
        point)))
