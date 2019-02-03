(ns scratchpad.tzone
  (:require
   [org.httpkit.client            :as http]
   [clj-etl-utils.landmark-parser :as lp]
   [clojure.java.io               :as io]
   [clojure.string                :as string]
   [clojure.data.csv              :as csv]))


;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(defonce page-data (slurp "https://en.wikipedia.org/wiki/List_of_tz_database_time_zones"))

(defn strip-html [s]
  (.text (org.jsoup.Jsoup/parse s)))

(defn sql-safe-string [s]
  (->
   s
   strip-html
   (.replaceAll "'" "''")))


(comment

  (->>
   clean-cc-rows
   (filterv #(.startsWith (nth % 0) "Cote "))
   first
   (mapv sql-safe-string))

  (mapv strip-html cc-rows)

  )




(comment
  (count page-data)
  (def parser (lp/make-parser page-data))

  (def segment (lp/extract parser [[:fp ">List</span>"] [:ft "<table"]] [[:ft "Number of zones"] [:rt "</table>"]]))

  (spit "segment.html"
        (lp/extract
         (lp/make-parser page-data)
         [[:fp ">List</span>"]
          [:ft "<table"]]
         [[:ft "Number of zones"]
          [:rt "</table>"]]))


  (def zone-rows
    (lp/html-table->matrix
     (lp/extract
      (lp/make-parser page-data)
      [[:fp ">List</span>"]
       [:ft "<table"]]
      [[:ft "Number of zones"]
       [:rt "</table>"]])))

  (with-open [wtr (io/writer "iso-tzone-data.tab")]
    (.write wtr (string/join "\t" ["CC" "Coordinates" "TZ" "Comments" "Format" "UTC OFFSET" "UTC DST OFSET" "Notes"]))
    (.write wtr "\n")
    (doseq
        [zone-row zone-rows]
      ;; [[cc coords tz comments format utc-offset utc-dst-offset notes] zone-rows]
      (when (and (>
                  (count zone-row) 2)
                 (not (empty? (nth zone-row 0)))
                 (nth zone-row 1))
        (.write wtr (string/join "\t" (mapv strip-html zone-row)))
        (.write wtr "\n"))))

  (with-open [wtr (io/writer "iso-tzone-data.psql")]
    (doseq
        [zone-row zone-rows]
      ;; [[cc coords tz comments format utc-offset utc-dst-offset notes] zone-rows]
      (when (and (>
                  (count zone-row) 2)
                 (not (empty? (nth zone-row 0)))
                 (nth zone-row 1))
        (.write wtr (apply format "INSERT INTO refdata.iso_timezones (country_code,coordinates,timezone,comments,format,utc_offset,utc_dst_offset,notes)\nVALUES ('%s','%s','%s','%s','%s','%s','%s','%s');\n"
                           (mapv sql-safe-string zone-row)))
        (.write wtr "\n"))))


  (defonce country-code-page (slurp "https://en.wikipedia.org/wiki/ISO_3166-1"))

  (def cc-table-data (lp/extract
                      (lp/make-parser country-code-page)
                      [[:fp ">Officially assigned code elements</span>"]
                       [:fp ">Officially assigned code elements</span>"]
                       [:ft "<table"]]
                      [[:ft ">Reserved and user-assigned code elements</span>"]
                       [:rt "</table>"]]))
  
  (spit "segment.html" cc-table-data)

  (def cc-rows (lp/html-table->matrix cc-table-data))

  (def clean-cc-rows
    (->>
     cc-rows
     (map #(mapv strip-html %))
     (filter #(and (> (count %) 1) (nth % 1)))
     vec))

  
  (with-open [wtr (io/writer "iso-country-code-data.tab")]
    ;; english-short name alpha2code alpha3code numeric-code subdivsion-code independent
    (.write
     wtr
     (string/join
      "\t"
      ["English Short Name" "Alpha 2 Code" "Alpha 3 Code" "Numeric Code" "Subdivision" "Independent"]))
    (.write wtr "\n")
    (doseq
        [cc-row cc-rows]
      (when (first cc-row)
        (.write wtr (string/join "\t" (mapv strip-html cc-row)))
        (.write wtr "\n"))))


  (with-open [wtr (io/writer "iso-country-code-data.psql")]
    (doseq
        [cc-row cc-rows]
      (when (and (> (count cc-row) 1) (nth cc-row 1))
        (.write wtr (apply format "INSERT INTO refdata.iso_countries (short_name,alpha2code,alpha3code,numeric_code,subdivision,independent)\nVALUES ('%s','%s','%s','%s','%s','%s');\n"
                           (mapv sql-safe-string cc-row)))
        (.write wtr "\n"))))


  (def currencies (slurp "https://raw.githubusercontent.com/datasets/currency-codes/master/data/codes-all.csv"))

  (spit "iso-currency-codes-all.csv" currencies)

  (def currency-recs
    (with-open [rdr (java.io.StringReader. currencies)]
      (doall (csv/read-csv rdr))))

  (with-open [wtr (io/writer "iso-currency-codes-all.psql")]
    (doseq [currency-rec (drop 1 currency-recs)]
      (.write wtr (apply format "INSERT INTO refdata.iso_currencies (entity,currency,alphabetic_code,numeric_code,minor_unit,withdrawl_date) VALUES ('%s','%s','%s','%s','%s','%s');\n"
                          (mapv sql-safe-string currency-rec)))))

  (->>
   currency-recs
   (filter #(< (count %) 5)))

  )
