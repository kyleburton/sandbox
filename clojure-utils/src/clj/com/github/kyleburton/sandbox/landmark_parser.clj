(ns com.github.kyleburton.sandbox.landmark-parser
  (:use [clojure.contrib.str-utils :as str]
        [clojure.contrib.fcase :only (case)]))

(defstruct parser :pos :doc :ldoc :doclen)

(defn make-parser [#^String doc]
  (struct-map parser
    :pos (atom 0)
    :ldoc (.toLowerCase doc)
    :doclen (.length doc)
    :doc doc))

(defn forward-past [parser landmark]
  (let [pos (.indexOf (:ldoc parser) (.toLowerCase landmark) @(:pos parser))]
    (if (= -1 pos)
      false
      (do
        (reset! (:pos parser) (+ pos (count landmark)))
        @(:pos parser)))))

(defn forward-to [parser landmark]
  (let [start (:post parser)
        pos (.indexOf (:ldoc parser) (.toLowerCase landmark) @(:pos parser))]
    (if (= -1 pos)
      false
      (do
        (reset! (:pos parser) pos)
        @(:pos parser)))))

(defn set-pos! [parser pos]
  (if (or (> pos (:doclen parser))
          (< pos 0))
    false
    (do
      (reset! (:pos parser) pos)
      true)))

(defn forward [parser cnt]
  (let [pos (+ cnt @(:pos parser))]
    (if (> pos (:doclen parser))
      false
      (do
        (reset! (:pos parser) pos)
        true))))

(defn rewind [parser cnt]
  (let [pos (- @(:pos parser) cnt)]
    (if (< pos 0)
      false
      (do
        (reset! (:pos parser) pos)
        true))))


(defn rewind-to [p landmark]
  (let [pos (.lastIndexOf (:ldoc p)
                          (.toLowerCase landmark)
                          @(:pos p))]
    (if (= -1 pos)
      false
      (do
        (reset! (:pos p) (+ pos (count landmark)))
        @(:pos p)))))

(defn rewind-past [p landmark]
  (let [pos (.lastIndexOf (:ldoc p)
                          (.toLowerCase landmark)
                          @(:pos p))]
    (if (= -1 pos)
      false
      (do
        (reset! (:pos p) pos)
        @(:pos p)))))

(def *cmds*
     {:apply-commands  apply-commands
      :a               apply-commands
      :do-commands     do-commands
      :d               do-commands
      :forward         forward
      :f               forward
      :forward-to      forward-to
      :ft              forward-to
      :forward-past    forward-past 
      :fp              forward-past
      :rewind          rewind
      :r               rewind
      :rewind-to       rewind-to
      :rt              rewind-to
      :rewind-past     rewind-past
      :rp              rewind-past})


(defn apply-commands [parser & cmds]
  (loop [[[cmd & args] & cmds] cmds]
    (if cmd
      (do
        ;(prn (format  "cmd=%s args=%s" cmd args))
        (if (apply (*cmds* cmd) (cons parser args))
          (do
            (recur cmds))
          false))
      true)))

(defn do-commands [parser cmds]
  (loop [[[cmd & args] & cmds] cmds]
    (if cmd
      (do
        ;(prn (format  "pos:%d cmd=%s args=%s" @(:pos parser) cmd args))
        (if (apply (*cmds* cmd) (cons parser args))
          (do
            ;(prn (format  "pos:%d cmd=%s args=%s" @(:pos parser) cmd args))
            (recur cmds))
          false))
      true)))

;; TODO: forward-to-regex, forward-past-regex, rewind-to-regex, rewind-past-regex

(defn doc-substr [parser cnt]
  (.substring (:doc parser)
              @(:pos parser)
              (+ @(:pos parser)
                 cnt)))

(defn extract [p start-cmds end-cmds]
  (let [orig-pos @(:pos p)]
    ;(prn (format "running start-cmds from:%d looking for %s" orig-pos start-cmds))
    (if (do-commands p start-cmds)
      (let [spos @(:pos p)]
        ;(prn (format "found start at:%d looking for end %s" spos end-cmds))
        (if (do-commands p end-cmds)
          (.substring (:doc p)
                      spos
                      @(:pos p))
          (do (set-pos! p orig-pos)
              false)))
      (do (set-pos! p orig-pos)
          false))))

(defn extract-from [html start-cmds end-cmds]
  (extract (make-parser html) start-cmds end-cmds))

(defn extract-all [p start-cmds end-cmds]
  (loop [res []]
    (if (do-commands p start-cmds)
      (let [spos @(:pos p)]
        (if (do-commands p end-cmds)
          (recur (conj res (.substring (:doc p) spos @(:pos p))))
          res))
      res)))

(defn extract-all-from [html start-cmds end-cmds]
  (extract-all (make-parser html) start-cmds end-cmds))

(defn table-rows [html]
  (extract-all-from html
                    '((:ft "<tr"))
                    '((:fp "</tr"))))

(defn row->cells [html]
  (extract-all-from html
                    '((:fp "<td")
                      (:fp ">"))
                    '((:ft "</td>"))))

