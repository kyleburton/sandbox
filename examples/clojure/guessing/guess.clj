

(defn make-guess [fn-is-correct? min max]
  (let [range (- max min)
        guess (+ min (long (* range (Math/random))))
        result (fn-is-correct? guess)]
    (cond
      (= -1 result)
      (lazy-cat [guess] (make-guess fn-is-correct? min guess))
      (= 1 result)
      (lazy-cat [guess] (make-guess fn-is-correct? guess max))
      (= 0 result)
      (repeat guess))))


(defn seq-of-guesses [fn-is-correct? guess-fn min max]
  (let [guess  (guess-fn min max)
        result (fn-is-correct? guess)]
    (cond
      (= -1 result)
      (lazy-cat [guess] (seq-of-guesses fn-is-correct? guess-fn min guess))
      (= 1 result)
      (lazy-cat [guess] (seq-of-guesses fn-is-correct? guess-fn guess max))
      (= 0 result)
      (repeat guess))))

(defn rand-in-range [min max]
  (+ min (long (* (- (inc max) min) (Math/random)))))

(defn split-range [min max]
  (long (/ (+ min max) 2)))

(defn inc-towards-max [min max]
  (inc min))

(defn sqrt-range [min max]
  (+ min (long (Math/sqrt (- max min)))))

(comment
  (rand-in-range 1 2)
  (split-range 1 3)
  (inc-towards-max 1 5)
  (sqrt-range 1 5)

  (let [answer        99
        check-answer (fn [x] (Long/compare answer x))
        min          1
        max          100]
    (take 50
          (seq-of-guesses
           check-answer
           #_rand-in-range
           #_split-range
           #_inc-towards-max
           sqrt-range
           min
           max)))


)