<h2 class="alert alert-success">Congratulations, your <a class="alert-link" href="http://luminusweb.net">Luminus</a> site is ready!</h2>

This page will help guide you through the first steps of building your site.

#### Why are you seeing this page?

The `home-routes` handler in the `text-adventure-engine.routes.home` namespace
defines the route that invokes the `home-page` function whenever an HTTP
request is made to the `/` URI using the `GET` method.

```
(defroutes home-routes
  (GET "/" []
       (home-page))
  (GET "/docs" []
       (-> (response/ok (-> "docs/docs.md" io/resource slurp))
           (response/header "Content-Type" "text/plain; charset=utf-8"))))
```

The `home-page` function will in turn call the `text-adventure-engine.layout/render` function
to render the HTML content:

```
(defn home-page []
  (layout/render "home.html"))
```

The page contains a link to the compiled ClojureScript found in the `target/cljsbuild/public` folder:

```
{% script "/js/app.js" %}
```

The rest of this page is rendered by ClojureScript found in the `src/cljs/text_adventure_engine/core.cljs` file.



#### Organizing the routes

The routes are aggregated and wrapped with middleware in the `text-adventure-engine.handler` namespace:

```
(def app-routes
  (routes
    (-> #'home-routes
        (wrap-routes middleware/wrap-csrf)
        (wrap-routes middleware/wrap-formats))
    (route/not-found
      (:body
        (error-page {:status 404
                     :title "page not found"})))))
```

The `app-routes` definition groups all the routes in the application into a single handler.
A default route group is added to handle the `404` case.

<a class="btn btn-primary" href="http://www.luminusweb.net/docs/routes.md">learn more about routing »</a>

The `home-routes` are wrapped with two middleware functions. The first enables CSRF protection.
The second takes care of serializing and deserializing various encoding formats, such as JSON.

#### Managing your middleware

Request middleware functions are located under the `text-adventure-engine.middleware` namespace.

This namespace is reserved for any custom middleware for the application. Some default middleware is
already defined here. The middleware is assembled in the `wrap-base` function.

Middleware used for development is placed in the `text-adventure-engine.dev-middleware` namespace found in
the `env/dev/clj/` source path.

<a class="btn btn-primary" href="http://www.luminusweb.net/docs/middleware.md">learn more about middleware »</a>




#### Need some help?

Visit the [official documentation](http://www.luminusweb.net/docs) for examples
on how to accomplish common tasks with Luminus. The `#luminus` channel on the [Clojurians Slack](http://clojurians.net/) and [Google Group](https://groups.google.com/forum/#!forum/luminusweb) are both great places to seek help and discuss projects with other users.
