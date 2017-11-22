(defpackage :com.github.kyleburton.scratch
  (:use :com.github.kyleburton.mylib
	:common-lisp))
(in-package :com.github.kyleburton.scratch)


;; threading

(defvar *global* 1)

(defun thunk-1 ()
  (loop for ii from 0 upto 10
     do
       (format t "thunk-1[ii=~a].before: *global*=~a~&" ii *global*)
       (setf *global* 5)
       (format t "thunk-1[ii=~a].after:  *global*=~a~&" ii *global*)))

;; (thunk-1)


;; NB: the mt package is only enabled if clisp is compiled with
;; --with-threads
;; and is considered experimental :(

;; (use-package 'mt)
;; (mt:make-thread #'thunk-1)
