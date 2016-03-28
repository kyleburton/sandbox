(ns scratchpad.primes)

(defonce cache (atom {:list [2 3 5 7]
                      :set  #{2 3 5 7}
                      :max  7}))

(defn reset-cache! []
  (reset!
   cache
   {:list [2 3 5 7]
    :set  #{2 3 5 7}
    :max  7}))

(defn add-prime! [^long p]
  (let [state   @cache
        new-max (if (> p (:max state))
                  p
                  (:max state))]
    (when-not (-> state :set (contains? p))
      (reset! cache
              (->
               state
               (update-in [:list] conj p)
               (update-in [:set] conj p)
               (assoc :max new-max))))))

(defn is-prime-and-why? [^long num]
  (let [cache-max ^long (-> cache deref :max)
        sqrt      ^long (-> num Math/sqrt Math/ceil long)]
    (cond
      (-> cache deref :set (contains? num))
      [true num :in-cache nil]

      (= 0 (mod num 2))
      [false num :div-by-2 2]

      (->>
       (-> cache deref :list)
       (filter #(= 0 (mod num %)))
       empty?
       not)
      [false num :div-by-some-already-computed-prime nil]

      :test-upto-sqrt
      (loop [^long idx (+ 2 cache-max)]
        (cond
          (> idx sqrt)
          (do
            (add-prime! num)
            [true num :hit-sqrt nil])
          
          (= 0 (mod num idx))
          [false num :divisible-by-number idx]
          
          :otherwise
          (recur (+ 2 idx)))))))

(defn is-prime? [^long num]
  (first (is-prime-and-why? num)))

(defn primes-seq []
  (let [max (-> cache deref :max)]
    (concat
     (-> cache deref :list)
     (filter is-prime? (iterate #(+ 2 %) (+ 2 max))))))

(defn last-digits-of-primes []
  (->>
   (primes-seq)
   (pmap #(mod % 10))))

(defn pairs-of-last-digits []
  (let [s1 (last-digits-of-primes)
        s2 (drop 1 s1)]
    (pmap (fn [x y] [x y])
          s1
          s2)))


(comment
  (reset-cache!)

  (is-prime-and-why? 27)
  (is-prime? 27)

  (->>
   (primes-seq)
   (take 10))
  
  (->>
   (range 300 400)
   (map is-prime-and-why?)
   (filter first))

  (->>
   (last-digits-of-primes)
   (take 100))

  (->>
   (pairs-of-last-digits)
   (take 100))
  
  (time
   (->>
    (pairs-of-last-digits)
    (drop 3) ;; skip 2, 3 and 5 (2 and 5 are outliers)
    (take 70000)
    (filter (fn [[x y]] (= 1 y)))
    (reduce
     (fn counter [acc pair]
       (update-in acc [pair] #(inc (or % 0))))
     {})
    #_(sort-by (fn [[[x y] count]] [x y count]))
    (sort-by (fn [[[x y] count]] [y x count]))))

  (->>
   cache
   deref
   :list
   count)
  )


