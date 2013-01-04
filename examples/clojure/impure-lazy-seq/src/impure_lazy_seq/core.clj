(ns impure-lazy-seq.core)

(def id-counter (java.util.concurrent.atomic.AtomicLong.))

(defn id-gen []
  (cons
   (.getAndIncrement id-counter)
   (lazy-seq
     (id-gen))))

(def id-seq (id-gen))

(take 3 id-seq)



(take 3 (id-gen))


(def custom-seq
     (reify clojure.lang.ISeq
            (first [this] (.getAndIncrement id-counter))
            (next  [this] (.getAndIncrement id-counter))
            (cons  [this thing]
                   (cons thing this))
            (more [this] (cons
                          (.getAndIncrement id-counter)
                          this))
            (count [this] (throw (RuntimeException. "count: not supported")))
            (empty [this] (throw (RuntimeException. "empty: not supported")))
            (equiv [this obj] (throw (RuntimeException. "equiv: not supported")))
            (seq   [this] this)))

(take 3 custom-seq)

