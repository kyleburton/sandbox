(ns com.github.kyleburton.clj-xpath
  (:require
   [clojure.contrib.str-utils :as str-utils])
  (:import
   [java.io                     InputStreamReader StringReader File IOException ByteArrayInputStream]
   [org.xml.sax                 InputSource SAXException]
   [javax.xml.transform         Source]
   [javax.xml.transform.stream  StreamSource]
   [javax.xml.validation        SchemaFactory]
   [org.w3c.dom                 Document Node]
   [javax.xml.parsers           DocumentBuilderFactory]
   [javax.xml.xpath             XPathFactory XPathConstants]))

(defn throwf [& args]
  (throw (RuntimeException. (apply format args))))

(defn node-list->seq [node-list]
  (loop [length (.getLength node-list)
         idx    0
         res    []]
    (if (>= idx length)
      (reverse res)
      (recur length
             (inc idx)
             (cons (.item node-list idx) res)))))

(defn xml-bytes->dom [bytes]
  (let [dom-factory (doto (DocumentBuilderFactory/newInstance)
                      (.setNamespaceAware true))
        builder     (.newDocumentBuilder dom-factory)
        rdr         (ByteArrayInputStream. bytes)]
    (.parse builder rdr)))

(defmulti $x (fn [xp xml-thing] (class xml-thing)))

(defmethod $x String [xp xml]
  ($x xp (xml-bytes->dom (.getBytes xml))))

(defmethod $x (Class/forName "[B") [xp bytes]
  ($x xp (xml-bytes->dom bytes)))

;; assume a Document (or api compatible)
(defmethod $x :default [xpath-string doc]
  (let [xpath-fact  (XPathFactory/newInstance)
        xp          (.newXPath xpath-fact)
        xpexpr      (.compile xp xpath-string)]
    (node-list->seq (.evaluate xpexpr doc XPathConstants/NODESET))))

(defn attrs [nodeattrs]
  (loop [[node & nodes] (node-list->seq (.getAttributes nodeattrs))
         res {}]
    (if node
      (recur nodes (assoc res (keyword (.getNodeName node)) (.getTextContent node)))
      res)))

(defn text [#^Node node]
  (.getTextContent node))

(defn summarize [s len]
  (if (>= len (.length s))
    s
    (str (.substring s 0 len) "...")))

(defn $x->tags [xp xml]
  (map #(keyword (.getNodeName %))
       ($x xp xml)))

(defn $x->tag [xp xml]
  (let [res ($x->tags xp xml)]
    (if (not (= 1 (count res)))
      (throwf "Error, more than 1 result (%d) from xml(%s) for xpath(%s)"
                    (count res)
                    (summarize xml 10)
                    xp))
    (first res)))

(defn $x->texts [xp xml]
  (map text ($x xp xml)))

(defn $x->text [xp xml]
  (let [res ($x->texts xp xml)]
    (if (not (= 1 (count res)))
      (throwf "Error, more than 1 result (%d) from xml(%s) for xpath(%s)"
                    (count res)
                    (summarize xml 10)
                    xp))
    (first res)))


(defn $x->attr-from-nodes [xp xml attr-name]
  (map (if (keyword? attr-name) attr-name (keyword attr-name)) (map attrs ($x xp xml))))

(defn $x->attrs [xp xml]
  (let [res (map attrs ($x xp xml))]
    (if (not (= 1 (count res)))
      (throwf "Error, more than 1 result (%d) from xml(%s) for xpath(%s)"
              (count res)
              (summarize xml 10)
              xp))
    (first res)))


(defmulti format-tag (fn [arg & [with-attrs]] (class arg)))

(defn format-tag-seq [tag-and-attrs & [with-attrs]]
  (if with-attrs
    (let [[tag & attrs] tag-and-attrs]
      (format "%s %s" (name tag)
              (str-utils/str-join " " (map (fn [[key val]]
                                             (format "%s=\"%s\"" (if (keyword? key) (name key) key) val))
                                           (partition 2 attrs)))))
    (name (first tag-and-attrs))))

(defmethod format-tag clojure.lang.PersistentVector [tag-and-attrs & [with-attrs]]
  (format-tag-seq tag-and-attrs with-attrs))

(defmethod format-tag clojure.lang.LazilyPersistentVector [tag-and-attrs & [with-attrs]]
  (format-tag-seq tag-and-attrs with-attrs))

(defmethod format-tag clojure.lang.Keyword [tag & [_]]
  (name tag))

(defmethod format-tag :default [tag & [_]]
  (str tag))


(defn tag [tag & body]
  (format "<%s>%s</%s>" (format-tag tag :with-attrs) (apply str body) (format-tag tag)))


(comment

  (tag :foo "body")
  (tag [:foo :name "bobby tables"] "select me from where I come from")

  ($x "/*" (tag [:foo :name "bobby tables"] "select me from where I come from"))

  ($x->tag "/" (slurp "/Users/kburton/personal/projects/sandbox/clj-xpath/pom.xml"))

)
