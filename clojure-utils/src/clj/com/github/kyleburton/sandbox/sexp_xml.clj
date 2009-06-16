;; TODO: missing ligature and character escaping/uri-escaping and
;; probably lots of other features...

(ns com.github.kyleburton.sandbox.sexp-xml
  (:use [clojure.contrib.str-utils :as str-utils]
        [com.github.kyleburton.sandbox.utils :as kutils]))


(defn- is-node? [node]
  (and (map? node)
       (contains? node
                  :tag)))

(defn format-attrs [node]
  (if (empty? node)
    ""
    (str " " (str-utils/str-join " " (map #(format "%s=\"%s\"" (name (first %)) (second %))
                                          (:attrs node))))))

(defn sexp->xml [tree]
  (loop [sb (StringBuffer.)
         tree   tree]
    (cond (is-node? tree)
          (do
            (.append sb (format "<%s%s>" (name (:tag tree)) (format-attrs tree)))
            (.append sb (sexp->xml (:content tree)))
            (.append sb (format "</%s>" (name (:tag tree))))
            (.toString sb))
          (vector? tree)
          (recur sb (seq tree))
          (seq? tree)
          (do
            (doseq [node tree]
              (.append sb (sexp->xml node)))
            (.toString sb))
          true
          (do
            (.toString sb)))))

(comment
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
    (println (sexp->xml xml)))

  )

