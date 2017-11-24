(ns the-mystery-on-orville-st.util
  (:require
   [clojure.tools.logging          :as log]
   [clojure.java.io                :as io]
   [clojure.java.shell             :as sh]
   [schema.core                    :as s]
   [the-mystery-on-orville-st.data :as data]))


;; utils to convert the rooms + edges into a graph

(def max-label-length 30)

(s/defn dot-name [s :- s/Str]
  (.replaceAll s "[^a-zA-Z0-9]" "_"))

(s/defn dot-label [s :- s/Str]
  (if (> (count s) max-label-length)
    (str (.substring s 0 (- max-label-length 3)) "...")
    s))

(s/defn rooms->dot [rooms :- {s/Keyword data/Room}]
  (doseq [[_ room] rooms]
    (printf "%s[label=\"%s\"];\n"
            (-> room :name name dot-name)
            (str
             (-> room :name name dot-name)
             ":"
             (-> room :description first dot-label)))))

;; TODO: eliminate duplicate edges
(s/defn edges->dot [edges :- [data/Edge]]
  (doseq [edge edges]
    (printf "%s--%s[label=\"%s\"];\n"
            (-> edge :from name dot-name)
            (-> edge :to name dot-name)
            (dot-label ""))))

(s/defn ugraph->dot [rooms :- {s/Keyword data/Room} edges :- [data/Edge]]
  (print "graph {\n")
  (rooms->dot rooms)
  (edges->dot edges)
  (print "}\n"))


(s/defn ugraph->dot->file [rooms :- {s/Keyword data/Room} edges :- [data/Edge] fname :- s/Str]
  (spit
   fname
   (with-out-str
     (ugraph->dot rooms edges))))

(s/defn render-to-graph [rooms :- {s/Keyword data/Room} edges :- [data/Edge] fname :- s/Str]
  (ugraph->dot->file rooms edges fname)
  (sh/sh "dot" "-Tpng" "-o" (str fname ".png") fname))

(comment

  (do
    (data/init!)
    (render-to-graph @data/rooms @data/edges "house.dot"))

  (ugraph->dot->file @data/rooms @data/edges "house.dot")
  
  (ugraph->dot @data/rooms @data/edges)

  (rooms->dot @data/rooms)

  (edges->dot @data/edges)

  @data/edges

  )
