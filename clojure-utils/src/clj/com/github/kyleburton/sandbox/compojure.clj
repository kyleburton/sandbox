(ns com.github.kyleburton.sandbox.compojure
  (:use compojure))


(defn template-page [request title & body]
  (let [title (str "Compojure App - " title)]
    (html [:head [:title title]] 
          [:h1 title]
          (if-let [flash (-> request :mailx-params :flash)]
            [:div {:style "background: #FEE"}
             flash])
          body
          [:br]
          [:br]
          [:div {:style "text-align: center;"}
           [:a {:href "/"} "Home"]
           "|" [:a {:href "/items"}     "Items"]
           "|" [:a {:href "/about"}     "About"]])))

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

(defroutes my-app
  (GET  "/"                      index-page)
  (GET  "/items"                 items-page)
  (GET  "/item/:id"              item-page)
  (GET  "/about"                 about-page)

  (ANY "*"
    (page-not-found)))

(comment

  (run-server {:port 8080}
              "/*" 
              (servlet my-app))

  (stop)

)

