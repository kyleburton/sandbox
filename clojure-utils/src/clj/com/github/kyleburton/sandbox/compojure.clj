(ns com.github.kyleburton.sandbox.compojure
  (:use compojure))

(defn resource-link [lnk]
  (format "%s?%s" lnk (.getTime (java.util.Date.))))
(defn template-page [request title & body]
  (let [title (str "Compojure App - " title)]
    (html [:head [:title title]
           [:link {:href "/stylesheet/app.css?3"
                   :rel "stylesheet"
                   :type "text/css"}]] 
          [:div {:id "header"}
           [:h1 title]]
          (if-let [flash (-> request :mailx-params :flash)]
            [:div {:id "flash"}
             flash])
          [:div {:id "main-body"} body]
          [:div {:id "footer"}
           [:a {:href "/"} "Home"]
           " | " [:a {:href "/items"}     "Items"]
           " | " [:a {:href "/about"}     "About"]
           " | " [:a {:href "http://github.com/weavejester/compojure/tree/master"} "Compojure"]])))

(defn index-page [request]
  (template-page request "Home"
    [:div "Welcome."]))

(defn xlink-to [text controller & params]
  [:a {:href (format "/%s/%s" (name controller) (first params))}
   text])

(defn items-page [request]
  (template-page request "Items"
    [:div "Items."]
    [:ul
     (for [item-id (range 10)]
       [:li (xlink-to (str "Item: " item-id) :item item-id)])]))

(defn item-page [request]
  (let [item-id (-> request :route-params :id)]
    (template-page request (str "Item: " item-id)
      [:div "Item: " item-id])))


(defn about-page [request]
  (template-page request "About"
    [:div "Thisi is a " [:a {:href "http://github.com/weavejester/compojure/tree/master"} "Compojure"] " application."]))

;; (defn page-not-found [request]
;;   (template-page request "Error: invalid route."
;;       [:div "Error, page not found."]
;;       [:pre (str request)]))

(defn stylesheet-page [request]
  {:status 200
   :headers { "Content-Type" "text/css" }
   :body "
body { 
  background: #EEE;
}

#main-body {
  margin-left: 30%;
  margin-right: 30%;
  margin-top: 1em;
  margin-bottom: 1em;
  border-style: solid;
  border-width: 2px;
  border-color: #AAA;
  padding: 0.5em;
}

#header {
  text-align: left;
  background: #CCC;
  padding: 1em;
  margin-left: 30%;
  margin-right: 30%;
}

#footer {
  text-align: center;
  background: #CCC;
  margin-top: 1em;
  padding: 0.5em;
  margin-left: 30%;
  margin-right: 30%;
}

"})

;(stylesheet-page 0)

(defroutes my-app
  (GET  "/"                      index-page)
  (GET  "/items"                 items-page)
  (GET  "/item/:id"              item-page)
  (GET  "/about"                 about-page)
  (GET  "/stylesheet/app.css"    stylesheet-page)

  (ANY "*"
    (page-not-found)))

(comment

  (run-server {:port 8080}
              "/*" 
              (servlet my-app))

  (stop)

)

