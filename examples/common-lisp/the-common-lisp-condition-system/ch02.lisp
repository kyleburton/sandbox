(defvar *phonebook*
  '((:mom :parent)
    (:dad :parent)
    (:alice :classmate :csgo :homework)
    (:bob :classmate :homework)
    (:catherine :classmate :ex)
    (:dorothy :classmate :girlfriend :csgo)
    (:eric :classmate :homework)
    (:dentist)))

(defvar *hooks* '())

(defvar *csgo-launched-p* nil)

(defun ensure-csgo-launched (person)
  (when (member :csgo person)
    (unless *csgo-launched-p*
      (format t ";; Launching Counter Strike for ~A.~%" (first person))
      (setf *csgo-launched-p* t))))

(defun call-person (person)
  (format t ";; Calling ~A.~%" (first person)))



(defun call-people ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (catch :do-not-call
      (dolist (hook *hooks*)
        (funcall hook person))
      (call-person person))))

(defun skip-non-csgo-people (person)
  (unless (member :csgo person)
    (format t ";; Nope, not claling ~A.~%" (first person))
    (throw :do-not-call nil)))

(defun maybe-call-parent (person)
  (when (member :parent person)
    (when (zerop (random 2))
      (format t ";; Nah, not calling ~A this time.~%" (first person))
      (throw :do-not-call nil))))

(defun skip-non-parents (person)
  (unless (member :parent person)
    (throw :do-not-call nil)))

(defun skip-ex (person)
  (when (member :ex person)
    (throw :do-not-call nil)))

(defun wish-happy-holidays (person)
  (format t ";; Gonna wish ~A happy holidays!~%" (first person)))

'(

  (let ((*hooks* (list #'ensure-csgo-launched
                       #'skip-non-csgo-people)))
    (call-people))

  (progn
    (format t "~%~%")
    (let ((*hooks* (list #'maybe-call-parent
                         #'skip-non-parents)))
      (call-people)))

  (progn
    (format t "~%~%")
    (let ((*hooks* (list #'skip-ex
                         #'wish-happy-holidays)))
      (call-people)))

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(

  (progn
    (format t "~%~%")
    (let ((*hooks* (list #'wish-happy-holidays)))
      (let ((*hooks* (append (list #'skip-ex) *hooks*)))
        (call-people))))

  )


(defvar *before-hooks* '())
(defvar *after-hooks* '())


(defun call-people2 ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (catch :do-not-call
      (dolist (hook *before-hooks*)
        (funcall hook person))
      (call-person person)
      (dolist (hook *after-hooks*)
        (funcall hook person)))))

(defun call-girlfriend-again (person)
  (when (member :girlfriend person)
    (format t ";; Gonna call ~A again.~%" (first person))
    (call-person person)))


'(

  (let ((*after-hooks* (list #'call-girlfriend-again)))
    (call-people2))

  (let ((*before-hooks* (list #'ensure-csgo-launched))
        (*after-hooks* (list #'call-girlfriend-again)))
    (call-people2))

  )


(makunbound '*before-hooks*)
(makunbound '*after-hooks*)

(defvar *hooks* '())

(defun call-hooks (kind &rest arguments)
  (dolist (hook *hooks*)
    (destructuring-bind (hook-kind hook-function) hook
      (when (eq kind hook-kind)
        (apply hook-function arguments)))))


(defun call-people3 ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (catch :do-not-call
      (call-hooks 'before-call person)
      (call-person person)
      (call-hooks 'after-call person))))


'(
  (progn
    (format t "~%~%")
    (let ((*hooks* `((before-call ,#'ensure-csgo-launched)
                     (after-call  ,#'call-girlfriend-again))))
      (call-people3)))


  (progn
    (format t "~%~%")
    (let ((*hooks* `((before-call ,#'skip-ex)
                     (before-call ,#'ensure-csgo-launched)
                     (before-call ,#'wish-happy-holidays)
                     (after-call  ,#'call-girlfriend-again))))
      (call-people3)))
  )

(define-condition grave-mistake (error)
  ((%reason :reader reason :initarg :reason)))

'(
  (reason
   (make-condition 'grave-mistake :reason "some stuff"))

  )

(defun receive-phone-call (person)
  (format t ";; Answering a call from ~A.~%" (first person))
  (when (member :ex person)
    (format t ";; About to commit a grave mistake...~%")
    (signal 'grave-mistake :reason :about-to-call-your-ex)
    ;; (we do not want to be here)
    ))

(defun defuse-error (condition)
  (declare (ignore condition))
  (format t ";; Nope nope nope, not answering!~%")
  (throw :do-not-answer nil))

(defun defuse-grave-mistake (condition)
  (let ((reason (reason condition)))
    (format t ";; Nope nope nope, not answering - reason was, ~A!~%" reason))
  (throw :do-not-answer nil))

'(

  (handler-bind ((error #'defuse-error))
    (dolist (person *phonebook*)
      (catch :do-not-answer
        (receive-phone-call person))))

  )


(defun receive-phone-call2 (person)
  (format t ";; Answering a call from ~A.~%" (first person))
  (when (member :ex person)
    (error 'grave-mistake :reason :about-to-call-your-ex)
    ;; (format t ";; we will never get here.~%")
    ))


'(

  ;; NB: this doesn't allow a restart or return
  ;; reading through:
  ;;  https://lispcookbook.github.io/cl-cookbook/error_handling.html#restarts-interactive-choices-in-the-debugger
  (handler-bind ((grave-mistake (lambda (condition)
                                  (declare (ignore condition))
                                  (format t ";;    allowing the grave mistake ...~%"))))
    (dolist (person *phonebook*)
      (catch :do-not-answer
        (receive-phone-call2 person))))

  (let ((val 3))
    (assert (zerop val)
            (val)
            "val must be zero, please change it.")
    :ok)

  (handler-bind ((grave-mistake (lambda (condition)
                                  (declare (ignore condition))
                                  (throw :do-not-answer nil))))
    (dolist (person *phonebook*)
      (catch :do-not-answer
        (receive-phone-call2 person))))

  (progn
    (format t "~%")
    (dolist (person *phonebook*)
      (let ((call-result
              (handler-case (progn
                              (receive-phone-call2 person)
                              :answered)
                (grave-mistake (condition)
                  (format t ";; Nope, not this time: ~A~%" (reason condition))
                  :not-answered))))
        (format t ";;    call-result: ~A~%" call-result))))

  )


;;  https://lispcookbook.github.io/cl-cookbook/error_handling.html#restarts-interactive-choices-in-the-debugger
(defun divide-with-restarts (x y)
  (restart-case (/ x y)
    (return-zero ()  ;; <-- creates a new restart called "RETURN-ZERO"
      :report "Return 0"
      0)
    (divide-by-one ()
      :report "Divide by 1"
      (/ x 1))))


'(

  (divide-with-restarts 3 0)

  )

(defun prompt-new-value (prompt)
  (format *query-io* prompt) ;; *query-io*: the special stream to make user queries.
  (force-output *query-io*)  ;; Ensure the user sees what he types.
  (list (read *query-io*)))   ;; We must return a list.

(defun divide-with-restarts2 (x y)
  (restart-case (/ x y)
    (return-zero ()  ;; <-- creates a new restart called "RETURN-ZERO"
      :report "Return 0"
      0)
    (divide-by-one ()
      :report "Divide by 1"
      (/ x 1))
    (set-new-divisor (value)
      :report "Enter a new divisor"
      :interactive (lambda () (prompt-new-value "Please enter a new divisor: "))
      (divide-with-restarts x value))))

'(

  (divide-with-restarts2 3 0)

  )


(defun prompt-new-value2 (prompt)
  (list
   (let ((input
           ;; We capture the program's output to a string.
           (with-output-to-string (s)
             (let* ((*standard-output* s))
               (uiop:run-program `("zenity"
                                   "--forms"
                                   ,(format nil "--add-entry=~a" prompt))
                                 :output s)))))
     ;; We get a string and we want a number.
     ;; We could also use parse-integer, the parse-number library, etc.
     (read-from-string input))))

;; (prompt-new-value2 "thing: ")

'(

  (divide-with-restarts2 3 0)


  )

(defun divide-with-restarts3 (x y)
  (restart-case (/ x y)
    (return-zero () ;; <-- creates a new restart called "RETURN-ZERO"
      :report "Return 0"
      0)
    (divide-by-one ()
      :report "Divide by 1"
      (/ x 1))
    (set-new-divisor (value)
      :report "Enter a new divisor"
      :interactive (lambda () (prompt-new-value2 "Please enter a new divisor: "))
      (divide-with-restarts x value))))

(defun divide-and-handle-error (x y)
  (handler-bind
      ((division-by-zero (lambda (c)
                           (format t "Got error: ~a~%" c) ;; error-message
                           (format t "and will divide by 1~&")
                           (invoke-restart 'divide-by-one))))
    (divide-with-restarts3 x y)))

'(

  (divide-and-handle-error 3 0)

  )


;; Local Variables:
;; inferior-lisp-program: "sbcl"
;; eval: (paredit-mode)
;; End:
