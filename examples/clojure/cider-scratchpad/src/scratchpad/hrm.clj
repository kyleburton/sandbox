(ns scratchpad.hrm
  (:require
   [clojure.tools.logging         :as log]
   [clojure.string                :as string]
   [clj-etl-utils.sequences       :as etl-seq]
   [clj-etl-utils.landmark-parser :as lp]
   [scratchpad.heap               :as heap]
   [clojure.java.io               :as io]
   [schema.core                   :as s]
   [clojure.math.combinatorics    :as combo]
   [com.rpl.specter               :as specter]))

(declare compile!)

(comment
  {::inbox  nil    ;; seq of items (any)
   ::outbox nil    ;; seq of items (any)

   ::cptr   0      ;; code pointer, counter into code
   ::reg    nil    ;; single register, any value
   ::mem    nil    ;; memory, a vector
   ::halted false  ;; true if the program has halted (eg: read from an empty inbox)
   ::error  nil    ;; true if the program has errored
   ::labels nil    ;; map of lables to the cptr index

   ::code   []     ;; vec of instructions
   }
  )
(defn cptr++ [ctx]
  (update-in ctx [::cptr] #(inc (or % 0))))

(comment
  (cptr++ {::inbox  nil
           ::outbox nil

           ::cptr   0
           ::reg    nil
           ::mem    nil
           ::halted false
           ::labels nil

           ::code   []})
  )

(defn at-cptr [ctx]
  (let [idx (::cptr ctx)]
    (if (>= idx (-> ctx ::code count))
      nil
      (-> ctx ::code (nth idx)))))

(comment
  (at-cptr {::inbox  nil
            ::outbox nil

            ::cptr   1
            ::reg    nil
            ::mem    nil
            ::halted false
            ::labels nil

            ::code   [[::label ::start]
                      [::inbox]
                      [::outbox]
                      [::jmp ::start]]})
  )

(defn error [err-id & args]
  [err-id args])

(defn compile-vals [vals]
  (reduce
   (fn [acc [k v]]
     (assoc acc k
            (cond
              (map? v) v
              :else    {::val v})))
   {}
   vals))

(defn make-new-mem!
  ([]
   (make-new-mem! {::size 0 ::vals {}}))
  ([opts]
   (let [defaults (compile-vals (::vals opts))
         size     (::size opts 0)]
     {::size  size
      ::cells (reduce
               (fn [acc idx]
                 (def acc acc)
                 (def idx idx)
                 (def defaults defaults)
                 (assoc acc
                        idx (merge {::idx   idx
                                    ::val   nil
                                    ::label idx}
                                   (get defaults idx))))
               {}
               (range size))})))

(comment
  (make-new-mem! {::size 4 ::vals {0 :banana}})

  )

(defn mem->cells-vec [mem]
  (for [idx (range (::size mem))]
    (-> mem ::cells (get idx))))

(defn mem->vals-vec [mem]
  (for [idx (range (::size mem))]
    (-> mem ::cells (get idx) ::val)))

(comment
  (make-new-mem!)
  (mem->cells-vec
   (make-new-mem!
    {::size 16
     ::vals {15 {::val 0 ::label :zero}}}))

  )

(defn mem-set! [mem idx val]
  (update-in mem [::cells idx] assoc ::val val))

(defn mem-get [mem idx]
  (-> mem ::cells (get idx)))

(defn mem-get-val [mem idx]
  (-> mem (mem-get idx) ::val))

(defn mem-set-indirect! [mem iidx val]
  (let [idx (mem-get mem iidx)]
    (mem-set! mem idx val)))

(defn mem-get-indirect [mem iidx]
  (let [idx (mem-get mem iidx)]
    (mem-get mem idx)))

(defn mem-get-val-indirect [mem iidx]
  (let [idx (mem-get mem iidx)]
    (mem-get-val mem idx)))

(defn mem-size [mem]
  (::size mem))

(comment
  (make-new-mem!)
  (make-new-mem! {::size 16})
  (assoc [] 1 :b)

  )

(defn do-copyto [ctx idx val]
  (if (> idx (-> ctx ::mem mem-size))
    [:error (assoc ctx ::error
                   (error ::err-mem-copyto-idx-out-of-range ::idx idx ::val val ::size (-> ctx ::mem mem-size)))]
    [:ok    (->
             ctx
             cptr++
             (update-in [::mem] mem-set! idx val))]))

(defn do-copyto-indirect [ctx idx val]
  (if (> idx (-> ctx ::mem mem-size))
    [:error (assoc ctx ::error
                   (error ::err-mem-copyto-idx-out-of-range ::idx idx ::val val ::size (-> ctx ::mem mem-size)))]
    [:ok    (->
             ctx
             cptr++
             (update-in [::mem] mem-set-indirect! idx val))]))

(comment
  (do-copyto
   {::inbox  [0 1 2 3]
    ::outbox nil
    ::cptr   2
    ::reg    :banana
    ::mem    (make-new-mem! {::size 8})
    ::code   [[::label ::start]
              [::inbox]
              [::copyto 0]
              [::outbox]
              [::jmp ::start]]}
   0
   :banana)

  )

(defn do-copyfrom [ctx idx]
  (if (> idx (-> ctx ::mem mem-size))
    [:error (assoc ctx ::error
                   (error ::err-mem-copyfrom-idx-out-of-range ::idx idx ::size (-> ctx ::mem mem-size)))]
    [:ok    (->
             ctx
             cptr++
             (assoc ::reg (mem-get-val (::mem ctx) idx)))]))

(defn do-copyfrom-indirect [ctx idx]
  (if (> idx (-> ctx ::mem mem-size))
    [:error (assoc ctx ::error
                   (error ::err-mem-copyfrom-idx-out-of-range ::idx idx ::size (-> ctx ::mem mem-size)))]
    [:ok    (->
             ctx
             cptr++
             (assoc ::reg (mem-get-val-indirect (::mem ctx) idx)))]))

(comment
  (do-copyfrom
   {::inbox  [0 1 2 3]
    ::outbox nil
    ::cptr   2
    ::reg    nil
    ::mem    (make-new-mem! {::size 8 ::vals {0 :banana}})
    ::code   [[::label ::start]
              [::inbox]
              [::copyfrom 0]
              [::outbox]
              [::jmp ::start]]}
   0)

  )

(defn apply! [ctx ins args]
  (cond
    (= ::label ins)
    [::ok (-> ctx
              cptr++)]

    (and
     (= ::inbox ins)
     (empty? (::inbox ctx)))
    [::error (assoc ctx ::error (error ::err-inbox-is-empty))]

    (= ::inbox ins)
    [::ok (->
           ctx
           (assoc
            ::reg   (-> ctx ::inbox first)
            ::inbox (-> ctx ::inbox rest))
           cptr++)]

    (and (= ::outbox ins)
         (nil? (::reg ctx)))
    [::error (assoc ctx ::error (error ::err-outbox-reg-is-nil))]

    (= ::outbox ins)
    [::ok (->
           ctx
           (assoc
            ::reg   nil
            ::outbox (conj (::outbox ctx) (-> ctx ::reg)))
           cptr++)]

    (= ::jmp ins)
    [::ok (assoc ctx ::cptr (-> ctx ::labels (get (first args))))]

    (and
     (= ::copyfrom ins)
     (-> args count (= 1)))
    (do-copyfrom ctx (first args))

    (and
     (= ::copyfrom ins)
     (-> args count (= 2)))
    (do-copyfrom-indirect ctx (first args))

    (and
     (= ::copyto ins)
     (-> args count (= 1)))
    (do-copyto ctx (first args) (::reg ctx))

    (and
     (= ::copyto ins)
     (-> args count (= 2)))
    (do-copyto-indirect ctx (first args) (::reg ctx))

    (and
     (= ::jmpz ins)
     (= 0 (::reg ctx)))
    [::ok (assoc ctx ::cptr (-> ctx ::labels (get (first args))))]

    (= ::jmpz ins)
    [::ok (cptr++ ctx)]

    (and
     (= ::jmpn ins)
     (< (::reg ctx) 0))
    [::ok (assoc ctx ::cptr (-> ctx ::labels (get (first args))))]

    (= ::jmpn ins)
    [::ok (cptr++ ctx)]

    (= ::add ins)
    (let [val (+ (::reg ctx) (mem-get-val (::mem ctx) (first args)))]
      [::ok (-> ctx cptr++ (assoc ::reg val))])

    (= ::sub ins)
    (let [_   (def reg (::reg ctx))
          _   (def arg (mem-get-val (::mem ctx) (first args)))
          _   (def val val)
          val (- (::reg ctx) (mem-get-val (::mem ctx) (first args)))]
      [::ok (-> ctx cptr++ (assoc ::reg val))])

    (= ::bump+ ins)
    (let [val     (inc (mem-get-val (::mem ctx) (first args)))
          new-mem (mem-set! (::mem ctx) (first args) val)]
      [::ok (->
             ctx
             cptr++
             (assoc ::reg val ::mem new-mem))])

    (= ::bump- ins)
    (let [val     (dec (mem-get-val (::mem ctx) (first args)))
          new-mem (mem-set! (::mem ctx) (first args) val)]
      [::ok (->
             ctx
             cptr++
             (assoc ::reg val ::mem new-mem))])

    :else
    [::error (assoc ctx ::error (error ::err-invalid-instruction [ins args]))]))

(comment
  (apply!
   (compile!
    {::inbox  [0 1 2 3]
     ::outbox nil

     ::cptr   3
     ::reg    nil
     ::mem    nil
     ::halted false
     ::labels nil

     ::code   [[::label ::start]
               [::inbox]
               [::jmpz ::start]
               [::outbox]
               [::label ::done]]})
   ::jmpz
   [::start])

  (apply!
   (compile!
    {::inbox  [0 1 2 3]
     ::outbox nil

     ::cptr   3
     ::reg    nil
     ::mem    nil
     ::halted false
     ::labels nil

     ::code   [[::label ::start]
               [::inbox]
               [::outbox]
               [::jmp ::start]]})
   ::jmp
   [::start])
  )

(defn compile! [ctx]
  (let [ctx (assoc ctx ::lables {})
        ctx (assoc ctx ::inbox  (::inbox ctx []))
        ctx (assoc ctx ::outbox (::outbox ctx []))
        ctx (assoc ctx ::cptr   (::cptr ctx 0))
        ctx (assoc ctx ::reg    (::reg ctx nil))
        ctx (assoc ctx ::mem    (or (::mem ctx) (make-new-mem!)))]
    (loop [ctx                  ctx
           cptr                 0
           [[ins & args] & instructions] (::code ctx)]
      (cond
        (>= cptr (count (::code ctx)))
        ctx

        (= ::label ins)
        (recur
         (update-in ctx [::labels] assoc (first args) cptr)
         (inc cptr)
         instructions)

        :else
        (recur ctx
               (inc cptr)
               instructions )))))

(comment
  (compile!
   {::inbox  [0 1 2 3]
    ::outbox nil
    ::cptr   0
    ::reg    nil
    ::mem    nil
    ::code   [[::label ::start]
              [::inbox]
              [::label ::write-to-outbox]
              [::outbox]
              [::jmp ::start]
              [::label ::cant-get-here]]})
  )

(defn pp-ins [ins]
  (if ins
    (str ":" (name (first ins)) (string/join "" (rest ins)))
    "*invalid*"))


(defn pp-code [code]
  (mapv pp-ins code))

(defn pp-ctx [ctx]
  (format "{r=%s; i=%s; m=%s; c=%s}"
          (::reg ctx)
          (-> ctx at-cptr pp-ins)
          (-> ctx ::mem mem->vals-vec)
          (-> ctx ::code pp-code)))

(defn execute! [ctx]
  (loop [ctx           (compile! ctx)
         [ins & args]  (at-cptr ctx)
         max-ins-count 99]
    (log/infof "execute![max=%s] %s" max-ins-count (pp-ctx ctx))
    (cond
      (not ins)
      [::error (assoc ctx ::error (error ::err-ran-past-end-of-code))]

      (zero? max-ins-count)
      [::error (assoc ctx ::error (error ::err-recursion-risk-max-ins-count-reached))]

      (::halted ctx)
      [::halted ctx]

      (::error ctx)
      [::error ctx]

      (and (empty? (::inbox ctx))
           (= ::inbox ins))
      [::halted ctx]

      :else ;; execute the instruciton at ::cptr
      (let [[result new-ctx] (apply! ctx ins args)]
        (if (= ::error result)
          [result new-ctx] ;; halt
          (recur
           new-ctx
           (at-cptr new-ctx)
           (dec max-ins-count)))))))

(comment

  (execute!
   {::inbox []
    ::mem   (make-new-mem! {::size 8 ::vals {0 10
                                             1 0}})
    ::code  [[::label :start]
             [::copyfrom 1]
             [::sub 0]
             [::jmpz :done]
             [::bump+ 1]
             [::jmp :start]
             [::label :done]
             [::copyfrom 0]
             [::outbox]
             [::copyfrom 1]
             [::outbox]
             [::label :end]
             [::inbox]]})

  (execute!
   {::inbox  [0 -1 2 -3 4 -5 6 -7]
    ::mem    (make-new-mem! {::size 8})
    ::code   [[::label :start]
              [::inbox]
              [::jmpn :skip]
              [::outbox]
              [::label :skip]
              [::jmp :start]]})


  (execute!
   {::inbox  [0 1 2 3]
    ::outbox nil
    ::cptr   0
    ::reg    nil
    ::mem    (make-new-mem! {::size 8})
    ::code   [[::label ::start]
              [::inbox]
              [::copyto 0]
              [::outbox]
              [::jmp ::start]]})

  )
