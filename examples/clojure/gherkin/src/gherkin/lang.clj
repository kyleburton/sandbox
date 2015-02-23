(ns gherkin.lang
  (:require
   [clojure.tools.logging      :as log]
   [schema.core                :as s]
   [clojure.core.async         :refer [chan go >! <! close!]]))

(def empty-registry [])

(defonce registry (atom empty-registry))

;; (first (map :matcher @registry))

(defn reset-registry! []
  (reset! registry empty-registry))

(re-find (re-matcher #"Foob" "Foo"))

(s/defn handler-matches? [handler :- s/Any line :- s/Str]
  (let [line    (.replaceAll line "^(Given|When|Then|And)\\s+" "")
        matcher (:matcher handler)]
    (cond
     (string? matcher)
     [(= line matcher) handler []]
     
     (isa? java.util.regex.Pattern (class matcher))
     (let [m           (re-matcher matcher line)
           find-result (re-find m)]
       (cond
        (nil? find-result)
        [false nil []]
        (string? find-result)
        [true handler []]
        :has-args
        [true handler (vec (rest find-result))]))

     :otherwise
     (throw (RuntimeException. (format "Error: unknown type of matcher: %s" matcher))))))

(s/defn find-handler [line :- s/Str]
  (->>
   @registry
   (map #(handler-matches? %1 line))
   (filter #(first %1))
   first
   rest
   vec))

(defmacro Given [matcher args-list & body]
  `(swap!
    registry
    conj
    {:keyword    :Given
     :matcher   ~matcher
     :args-list '~args-list
     :body      '~body
     :fn        (fn ~args-list ~@body)}))

(defmacro When [matcher args-list & body]
  `(swap!
    registry
    conj
    {:keyword    :When
     :matcher   ~matcher
     :args-list '~args-list
     :body      '~body
     :fn        (fn ~args-list ~@body)}))

(defmacro Then [matcher args-list & body]
  `(swap!
    registry
    conj
    {:keyword    :Then
     :matcher   ~matcher
     :args-list '~args-list
     :body      '~body
     :fn        (fn ~args-list ~@body)}))

(defmacro And [matcher args-list & body]
  `(swap!
    registry
    conj
    {:keyword    :And
     :matcher   ~matcher
     :args-list '~args-list
     :body      '~body
     :fn        (fn ~args-list ~@body)}))

(def FeatureSpec
  {(s/optional-key :title)       s/Str
   (s/optional-key :description) s/Any
   (s/optional-key :lines)       [s/Any]})

(def GherkinSpec
  {(s/optional-key :features) [FeatureSpec]
   (s/optional-key :lines)    s/Any})

(s/defn trim-string :- s/Str [s :- (s/maybe s/Str)]
  (when s
    (.trim s)))

(defn is-scenario-line? [line block-type]
  (or
   (and
    (= :scenario block-type)
    (not (empty? line)))
   (and
    (= :none block-type)
    (.startsWith line "Scenario: "))))

(defn is-feature-line? [line block-type]
  (or
   (and
    (= :feature block-type)
    (not (empty? line)))
   (and
    (= :none block-type)
    (.startsWith line "Feature: "))))

(defn parse [text]
  (loop [[line & lines] (map trim-string (.split text "\\r?\\n"))
         lnum           1
         block-type     :none
         parsed         []]
    (cond
     (nil? line)
     {:status :ok
      :parsed parsed}
     
     ;; end the current block
     (empty? line)
     (recur lines (inc lnum) :none parsed)

     (is-feature-line? line block-type)
     (recur lines
            (inc lnum)
            :feature
            (conj
             parsed
             {:line           line
              :lnum           lnum
              :start-of-block (= :none block-type)
              :type           :feature}))

     (is-scenario-line? line block-type)
     (recur lines
            (inc lnum)
            :scenario
            (conj
             parsed
             {:line           line
              :lnum           lnum
              :start-of-block (= :none block-type)
              :type           :scenario}))

     :otherwise
     {:status  :error
      :message (format "Unknown line type starting at %s: '%s'" lnum line)
      :lnum    lnum
      :line    line
      :lines   lines
      :parsed  parsed})))

(defn parse-table-line [table-line]
  (->
   table-line
   :line
   (.replaceAll "^\\|" "")
   (.replaceAll "\\|$" "")
   .trim
   (.split "\\s*\\|\\s*")
   vec))

(defn keywordize-col-name [cname]
  (->
   cname
   (.replaceAll "[^a-zA-Z0-9]" "-")
   .toLowerCase
   keyword))

(defn table-lines->table [table-lines]
  (let [[header-line & lines] table-lines
        ;; NB: should we keywordize table column names?
        header                (map keywordize-col-name (parse-table-line header-line))
        rows                  (mapv parse-table-line lines)]
    (mapv
     #(zipmap header %1)
     rows)))

(defn scenario->steps [scenario]
  ;; NB: skip the first line, it's just a description
  (loop [[step & steps] (rest scenario)
         compiled-steps []]
    (cond
     (nil? step)
     compiled-steps

     ;; step with a table
     (and (-> steps first :line) "|"
          (.startsWith (-> steps first :line) "|"))
     (let [[table-lines remaining-steps] (split-with #(.startsWith (:line %1) "|") steps)]
       (recur
        remaining-steps
        (conj compiled-steps (assoc step :table-lines (table-lines->table table-lines)))))

     :just-a-step
     (recur steps (conj compiled-steps step)))))

(comment
  (scenario->steps scenario)
  )


(defn run-scenario [ctx scenario]
  (def scenario scenario)
  (loop [ctx            ctx
         [step & steps] (scenario->steps scenario)
         snum           1]
    (def step step)
    (let [[handler args] (find-handler (:line step))]
      (def handler handler)
      (def args args)
      (cond
       (not handler)
       [{:status   :error
         :message  (format "No Step exists for '%s'\n\n(%s #\"%s\" []\n  :body\n" (:line step) "Then" (:line step))
         :scenario scenario
         :step     step
         :snum     snum
         :ctx      ctx}
        ctx]

       :found-handler
       (let [[status new-ctx ex] (try
                                   (apply (:fn handler) ctx args)
                                   (catch Exception ex
                                     (def handler handler)
                                     (def ctx ctx)
                                     (def args args)
                                     (log/infof ex "Error executing handler: %s %s\n  args=%s" handler ex (vec args))
                                     [false ctx ex]))]
         (cond
          status
          (recur new-ctx steps (inc snum))
          
          (not (nil? ex))
          [{:status    :error
            :message   (format "Exception thrown: %s" ex)
            :exception ex
            :scenario  scenario
            :step      step
            :snum      snum
            :ctx       ctx}
           new-ctx]

          :step-failed
          [{:status   :fail
            :message  "Step failed."
            :scenario scenario
            :step     step
            :snum     snum
            :ctx      ctx}
           new-ctx]))))))

(comment
  (first (run-scenario {:example :context} scenario))
  )

(defn run-gherkin [ctx parsed-gherkin]
  (def ctx ctx)
  (def parsed-gherkin parsed-gherkin)
  (let [scenarios        (->>
                          (:parsed parsed-gherkin)
                          (partition-by :type)
                          (filter (fn [block]
                                    (= (-> block first :type) :scenario))))
        scenario-results (loop [ctx                  ctx
                                [scenario scenarios] scenarios
                                results              []]
                           (cond
                            (nil? scenario)
                            results

                            :otherwise
                            (let [[result new-ctx] (run-scenario ctx scenario)]
                              (recur new-ctx scenarios (conj results result)))))]
    {:spec             parsed-gherkin
     :scenario-results scenario-results}))

(comment

  (->
   (run-gherkin ctx parsed-gherkin))

  )
