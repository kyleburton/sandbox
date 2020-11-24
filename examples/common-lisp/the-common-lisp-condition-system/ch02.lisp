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

(defun ensure-csgo-launched (person)
  (when (member :csgo person)
    (unless *csgo-launched-p*
      (format t ";; Launching Counter Strike for ~A.~%" (first person))
      (setf *csgo-launched-p* t))))

(defun call-person (person)
  (format t ";; Calling ~A.~%" (first person)))


(defvar *csgo-launched-p* nil)

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



;; Local Variables:
;; inferior-lisp-program: "sbcl"
;; eval: (paredit-mode)
;; End:
