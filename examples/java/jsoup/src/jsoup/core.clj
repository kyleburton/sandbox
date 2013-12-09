(ns jsoup.core
  (:import
   [org.jsoup Jsoup]))

(comment


  (let [docid "24312906"
        uri (str "http://www.ncbi.nlm.nih.gov/pubmed/" docid)
        doc (.. (Jsoup/connect uri)
                (timeout 60000)
                (userAgent "Mozilla/25.0")
                get)
        authors (.select doc "div.auths >*")]
    (def thing
         {:doc doc
          :authors authors})
    thing)

  (:authors thing)


  (seq (.keySet (System/getenv)))

  (seq (System/getProperties))

)