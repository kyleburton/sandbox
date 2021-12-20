(ns scratchpad.quine)

;; see: http://malisper.me/2016/04/20/writing-self-referential-programs/



(defn code-that-generates [program]
  `'~program)

(def quine1-src '(fn [a]
                   (let [b `'~a]
                     `(~a ~b))))

;; (defun self-referential-version-of (f)
;;   `(,f
;;     ((lambda (a)
;;              (let ((b `',a))
;;                (let ((ab `(,a ,b)))
;;                  `(,',f ,ab))))
;;      '(lambda (a)
;;               (let ((b `',a))
;;                 (let ((ab `(,a ,b)))
;;                                 `(,',f ,ab)))))))

(defn self-referential-version-of [f]
  `(~f
    ((fn [a]
             (let [b `'~a]
               (let [ab `(~a ~b)]
                 `(~'~f ~ab))))
     '(fn [a]
              (let [b `'~a]
                (let ((ab `[~a ~b]))
                  `(~'~f ~ab)))))))


(comment
  ((eval quine1-src) quine1-src)

  (self-referential-version-of '(fn (x) (list x x)))


  )
