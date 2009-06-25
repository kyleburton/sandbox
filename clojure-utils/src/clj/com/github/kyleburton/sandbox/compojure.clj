(ns com.github.kyleburton.sandbox.compojure
  (:use compojure))

(defn resource-link [lnk]
  (format "%s?%s" lnk (.getTime (java.util.Date.))))

(defmacro mresource-link [lnk]
  (format "%s?%s" lnk (.getTime (java.util.Date.))))

(defn template-page [request title & body]
  (let [title (str "Compojure App - " title)]
    (html [:head [:title title]
           [:link {:href (mresource-link "/stylesheet/app.css")
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
      [:div "Item: " item-id]
      [:div "Request: " 
       [:table {:id "session-dump"} [:tr [:th "Key"]
                [:th "Value"]
                (map (fn [key] 
                       [:tr [:td key] [:td (request key)]])
                     (keys request))]]])))


(defn about-page [request]
  (template-page request "About"
    [:div "This is a " [:a {:href "http://github.com/weavejester/compojure/tree/master"} "Compojure"] " application."]))

(defn stylesheet-page [request]
  {:status 200
   :headers { "Content-Type" "text/css" }
   :body "
body { 
  background: #EEE;
}

#main-body {
  margin-left: 20%;
  margin-right: 20%;
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
  margin-left: 20%;
  margin-right: 20%;
}

#footer {
  text-align: center;
  background: #CCC;
  margin-top: 1em;
  padding: 0.5em;
  margin-left: 20%;
  margin-right: 20%;
}

/*
#session-dump {
  border-width: 1px;
  border-style: outset;
  border-spacing: 2px;
  border-collapse: collapse; // separate
}

#session-dump td {
  border-width: 1px;
}
*/

"})

(defroutes my-app
  (GET  "/"                      index-page)
  (GET  "/items"                 items-page)
  (GET  "/item/:id"              item-page)
  (GET  "/about"                 about-page)
  (GET  "/stylesheet/app.css"    stylesheet-page)

  (ANY "*"
    (page-not-found)))

(comment

  ;; execute the next form to run the server
  (defserver *jetty* {:port 8080}
    "/*" 
    (servlet my-app))

  (start *jetty*)

  ;; execute to stop the server
  (stop *jetty*)

)

