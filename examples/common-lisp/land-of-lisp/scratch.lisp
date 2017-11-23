(defpackage :com.github.kyleburton.scratch
  (:use :com.github.kyleburton.mylib
	:common-lisp
	:sb-ext))
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





(comment
  (load "quicklisp.lisp")
  (load "./.quicklisp/setup.lisp")
  (ql:quickload :inferior-shell)
  

  (defvar *proc* (sb-ext:run-program "/bin/ls" nil))
  (defvar *proc*
    (let* (
	   (proc (sb-ext:run-program "/bin/ls" nil :search t)))))

  (sb-ext:process-p *proc*)
  (sb-ext:process-input *proc*)
  (sb-ext:process-output *proc*)
  (sb-ext:process-error *proc*)


  (inferior-shell:run/ss '(ls "-lFa" "./mylib.lisp"))

)
