(load "./.quicklisp/setup.lisp")

(defmacro comment (&body body)
  (declare (ignore body))
  nil)


;; From: http://danshapero.github.io/lisp/2015/01/22/threading-macro-in-cl.html
(defmacro -> (x &rest forms)
  (flet ((expand-form (x form)
	   (if (consp form)
	       (if (or (eq (car form) 'lambda)
		       (eq (car form) 'function))
		   `(funcall ,form ,x)
		   `(,(car form) ,x ,@ (cdr form)))
	       `(,form ,x))))
    (reduce #'expand-form forms :initial-value x)))


(defmacro ->> (x &rest forms)
  (flet ((expand-form (x form)
	   (if (consp form)
	       (if (or (eq (car form) 'lambda)
		       (eq (car form) 'function))
		   `(funcall ,form ,x)
		   ;; `(,(car form) ,x ,@ (cdr form))
		   `(,@form ,x))
	       `(,form ,x))))
    (reduce #'expand-form forms :initial-value x)))



(defun range (&rest args)
  (destructuring-bind (start max step)
      (cond
	((= 1 (length args))    (list            0 (first args)           1))
	((= 2 (length args))    (list (nth 0 args) (nth 1 args)           1))
	((= 3 (length args))    (list (nth 0 args) (nth 1 args)           (nth 2 args)))
	(t                      (list 0 0 1)))
    (cond
      ((= 0 step)     (signal 'step-must-be-non-zero))
      ((= start max)  '())
      ((< max start)  (loop for ii downfrom start to (1+ max) by (abs step) collect ii))
      (t              (loop for ii     from start to (1- max) by (abs step) collect ii)))))

;; (ql:quickload "vecto")
;; (apropos "sea" 'ql)

;; (ql:system-apropos "trivial-download")
;; (ql:system-apropos "asdf")
;; (ql:system-apropos "uiop")
;; (ql:system-apropos "json-rea")

;; (ql:quickload "quicklisp-slime-helper")
;; (ql:quickload "trivial-download")
;; (ql:uninstall "trivial-download")
;; (ql:uninstall "drakma")
;; (apropos "downlo")

;; (ql:quickload "trivial-download")
;; (trivial-download:download "https://gist.githubusercontent.com/chaitanyagupta/9324402/raw/54359d3f2d19b78c603cf011304a741410a873ae/json-reader.lisp" "json-reader.lisp")
;; (trivial-download:download "https://gist.githubusercontent.com/chaitanyagupta/9324402/raw/54359d3f2d19b78c603cf011304a741410a873ae/json-reader.lisp" "json-reader.lisp")
;; (apropos "download" 'trivial-download)



;; https://gist.github.com/chaitanyagupta/9324402


;; (range)
;; (range 10)
;; (range 4 14)       (4 5 6 7 8 9 10 11 12 13)
;; (range 4 14 3)     (4 7 10 13)
;; (range 10 0 -1)    (10 9 8 7 6 5 4 3 2 1)
;; (range 100 0 -17)  (100 83 66 49 32 15)
;; (range 100 0 12.5) (100 87.5 75.0 62.5 50.0 37.5 25.0 12.5)
;; (range 0 3 0.125)  (0 0.125 0.25 0.375 0.5 0.625 0.75 0.875 1.0 1.125 1.25 1.375 1.5 1.625 1.75 1.875 2.0)


;; (append '(1 2 3) '(4 5) '(6 7 8))            => (1 2 3 4 5 6 7 8)
;; (concatenate 'list '(1 2 3) '(4 5) '(6 7 8)) => (1 2 3 4 5 6 7 8)
;; (concatenate 'string "this" " " "that")      => "this that"
;; call-arguments-limit  => 4096 (maximum for fn's like apply)


;; TODO support multiple arity
(defmacro defonce (pname value)
  `(if (boundp ',pname)
       nil
       (defparameter ,pname ,value)))
