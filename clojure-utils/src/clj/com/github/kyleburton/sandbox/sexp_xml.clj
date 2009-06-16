;; TODO: missing ligature and character escaping/uri-escaping and
;; probably lots of other features...

(ns com.github.kyleburton.sandbox.sexp-xml
  (:use [clojure.contrib.str-utils :as str-utils]
        [com.github.kyleburton.sandbox.utils :as kutils]))


(def *xml-header* "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")

(def *indent* false)

(def *indent-prefix* " ")

(defn- is-node? [node]
  (and (map? node)
       (contains? node
                  :tag)))

(defn- format-attrs [node]
  (if (empty? node)
    ""
    (str " " (str-utils/str-join " " (map #(format "%s=\"%s\"" (name (first %)) (second %))
                                          (:attrs node))))))

(defn- line-end []
  (if *indent*
    "\n"
    ""))

(defn- indent [depth]
  (if *indent*
    (apply str (repeat depth *indent-prefix*))
    ""))

(defn- sexp->xml-aux [sb tree depth]
  (cond (is-node? tree)
        (do
          ;; TODO: if body is empty, make the tag end with ' />'
          ;; TODO: only end with \n if the body has nested tags, need
          ;;   to perfectly preserve body text
          (.append sb (format "%s<%s%s>%s" 
                              (indent depth)
                              (name (:tag tree))
                              (format-attrs tree)
                              (line-end)))
          (.append sb (sexp->xml-aux (StringBuilder.) (:content tree) (+ 1 depth)))
          (.append sb (format "%s</%s>%s" (indent depth) (name (:tag tree)) (line-end)))
          (.toString sb))
        (vector? tree)
        (recur sb (seq tree) (+ 1 depth))
        (seq? tree)
        (do
          (doseq [node tree]
            (.append sb (sexp->xml-aux (StringBuilder.) node (+ 1 depth))))
          (.toString sb))
        true
        (do
          (str tree))))

(defn sexp->xml 
  "Return the structure produced by clojure.xml/parse back into xml."
  [tree]
  (str *xml-header* (sexp->xml-aux (StringBuilder.) tree 0)))

(comment

  (binding [*indent* true]
    (let [xml-str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<profiles version=\"8\">
<profile name=\"HMS\" version=\"8\">
<setting id=\"org.eclipse.jdt.core.formatter.align_type_members_on_columns\" value=\"false\"/>
<setting id=\"org.eclipse.jdt.core.formatter.tabulation.size\" value=\"2\"/>
<setting id=\"org.eclipse.jdt.core.formatter.use_tabs_only_for_leading_indentations\" value=\"false\"/>
</profile>
</profiles>
"
          xml (clojure.xml/parse (org.xml.sax.InputSource. (java.io.StringReader. xml-str)))]
      xml
      (println "")
      (println (sexp->xml xml))))

  (binding [*indent* true] 
    (println (sexp->xml 
              (clojure.xml/parse
               (org.xml.sax.InputSource.
                (java.io.StringReader. 
                 "<?xml version=\"1.0\" encoding=\"UTF-8\"?><some-tag>This is some content</some-tag>"))))))

  )

