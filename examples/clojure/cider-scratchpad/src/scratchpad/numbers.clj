(ns scratchpad.numbers)

(comment


  (loop [arg 9
         ii  0
         max 99]
    (cond
      (> ii max)
      :done

      :otherwise
      (do
        (printf "%s * %s = %s\n" arg ii (* arg ii))
        (if (= 657 (* arg ii))
          (printf "LAUNCHING MISSILE TO RUSSIA!!!\n"))
        (recur arg (inc ii) max))))
  )

(defn sq [xx]
  (* xx xx))

(defn sqrt [xx]
  (Math/sqrt xx))

(defn is-right-triangle? [aa bb cc]
  (= (sq cc)
     (+ (sq aa) (sq bb))))

(defn is-right-triangle-with-details? [aa bb cc]
  {:aa aa
   :bb bb
   :cc cc
   :sq-aa (sq aa)
   :sq-bb (sq bb)
   :sq-cc (sq cc)
   :aa-sq-bb-sq (+ (sq aa) (sq bb))
   :delta (- (sq cc) (+ (sq aa) (sq bb)))
   :is-right-triangle? (is-right-triangle? aa bb cc)})

(defn find-cc [aa bb]
  (sqrt (+ (sq aa) (sq bb))))

(defn find-aa [bb cc]
  (sqrt (- (sq cc) (sq bb))))

(defn find-aa-with-details [bb cc]
  (let [aa (find-aa bb cc)]
    {:aa aa
     :bb bb
     :cc cc
     :sq-aa (sq aa)
     :sq-bb (sq bb)
     :sq-cc (sq cc)}))

(defn triangle->area
  ([aa bb _cc]
   (triangle->area aa bb))
  ([aa bb]
   (* 1/2 aa bb)))

(comment
  (find-cc 3 4)
  (find-aa 3 5)
  4.0
  (find-aa 4 5)
  3.0

  (is-right-triangle? 3 4 5)

  (triangle->area 3 4 5)
  6N
  (triangle->area 3 4)


  (find-aa 17 19)
  8.48528137423857

  (find-cc 5 14)
  14.866068747318506

  (find-aa 6.4 12.2)

  (is-right-triangle? 6.4 12 12.2)
  false

  (is-right-triangle-with-details? 6.4 12 12.2)

  (find-cc 8 5)
  9.433981132056603
  )
