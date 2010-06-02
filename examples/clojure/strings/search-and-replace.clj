(ns search-and-replace
  (import [java.util.regex Pattern Matcher]))

(defn replace-all [target m]
  (reduce
   (fn [#^String s [#^CharSequence what #^CharSequence with]]
     (.replaceAll s what with))
   target
   m))

(replace-all "this that other"
             {"t" "T"
              "r" "R"})


(defn replace-all-re [target m]
  (reduce
   (fn [#^String s [#^Pattern pat #^CharSequence with]]
     (.replaceAll (.matcher pat s) with))
   target
   m))


(replace-all-re "this that other part"
                [[#"\s+" "    "]
                 [#"t" "|"]
                 [#"\w+" "FOOF"]])

