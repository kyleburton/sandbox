(ns com.github.kyleburton.sandbox.selenium
  (:import  [com.thoughtworks.selenium DefaultSelenium])
  (:require [com.github.kyleburton.sandbox.log4j :as log]))

;; Plans
;;
;; * abstractions between jQuery, Prototype, and the other front-end
;;   javascript frameworks
;;
;; * dsl-ish helpers, make them as smart (meaning as easy to use) as possible:
;;
;;   ** click! anything that can receive a click
;;
;; * ide-ish features:
;;
;;   ** ask to see all the links, - don't return the list of HTML DOM
;;      nodes, rather return it as a sequence of '(click!
;;      <<locator>>)' forms so the user can select the one they want
;;      (cut & paste, maybe even click in a more advanced GUI).
;;
;;   ** make use of some simple SWING or SWT GUI widgets to perform
;;      some of the interaction -- if the library does this, it should
;;      try to detect if a GUI is present (eg, test for DISPLAY or
;;      being on Windows or a Mac)

(def *log* (log/logger *ns*))



(def *selenium-defaults* (atom
                          {:host    "localhost"
                           :port    4444
                           :browser "*firefox"
                           :url     "http://localhost/"}))

(defn new-selenium [& [host port browser url initial-url]]
  (let [sel (DefaultSelenium.
              (or host    (:host    @*selenium-defaults*))
              (or port    (:port    @*selenium-defaults*))
              (or browser (:browser @*selenium-defaults*))
              (or url     (:url     @*selenium-defaults*)))]
    sel))

(defn sel-jquery [sel js]
  (.getEval
   sel
   (format "this.browserbot.getCurrentWindow().%s" js)))

(defn sel-input [sel locator text]
  (.type sel locator text))

(defn sel-click [sel locator]
  (.click sel locator))

(defn sel-submit [sel locator]
    (.submit sel locator))

(defn jquery? [sel]
  (not (= "null" (.getEval sel "dom=selenium.browserbot.getCurrentWindow().jQuery"))))

(defn prototype? [sel]
  (not (= "null" (.getEval sel "dom=selenium.browserbot.getCurrentWindow().Amax"))))

(defn javascript! [sel & js]
  (.getEval sel (apply str js)))

(defn jquery! [sel & js]
  (.getEval sel (format "selenium.browserbot.getCurrentWindow().jQuery(%s)" (apply str js))))



(defmacro with-selenium [[name sel] & body]
  `(let [~name ~sel
         ~'jquery!  (fn [js#]   (.getEval   ~name js#))
         ~'jquery?  (fn []      (jquery? ~name))
         ~'type!    (fn [l# t#] (.type      ~name l# t#))
         ~'click!   (fn [loc#]  (.click     ~name loc#))
         ~'submit!  (fn [loc#]  (.submit    ~name loc#))
         ~'visible? (fn [loc#]  (.isVisible ~name loc#))]
     ~@body))

(comment

  (def *sel* (new-selenium))
  (.start *sel*)
  (.open *sel* "http://localhost/")
  (sel-input *sel* "login" "admin")
  (sel-input *sel* "password" "monkey")
  (sel-submit *sel* "//form")
  (sel-click *sel* "//area")
  (sel-jquery *sel* "$('#margin_demands_unsent_table').html()")
  (sel-click *sel* "//a[contains(text(),'Logout')]")

  ;(.start *sel*)
  ;; selenium.waitForCondition("selenium.browserbot.getCurrentWindow().Ajax.activeRequestCount == 0;", DEFAULT_WAIT_PERIOD);

  ;; jQuery:    jQuery.active
  ;; Prototype: Ajax.activeRequestCount

  (.getEval *sel* "this.browserbot.getCurrentWindow().jQuery.active")

  (.open *sel* "http://localhost/")

  ;; this is wait-for-ajax if we're using jQuery
  (.waitForCondition *sel* "this.browserbot.getCurrentWindow().jQuery.active == 0;")
  ;; this is what you use for prototype:
  ;; (.waitForCondition *sel* "this.browserbot.getCurrentWindow().Ajax.activeRequestCount == 0;")

  (with-selenium
      [sel *sel*]
    (when (.isElementPresent *sel* "//a[contains(text(),'Logout')]")
      (click! "//a[contains(text(),'Logout')]")
      (.waitForPageToLoad *sel* "500"))
    (when (not (.isElementPresent *sel* "//a[contains(text(),'Logout')]"))
      (.open *sel* "http://localhost")
      (.waitForPageToLoad *sel* "500"))
    ;(click! "//a[contains(text(),'Logout')]")
    (.waitForPageToLoad *sel* "500")
    (type! "login"    "admin")
    (type! "password" "monkey")
    (submit! "//form")
    (.waitForPageToLoad *sel* "500")
    (click! "//area")
  )

  (jquery? *sel*)
  (prototype? *sel*)

;;   (.getEval *sel* "selenium.browserbot.getCurrentWindow().$")
;;   (.getEval *sel* "this.browserbot.getCurrentWindow().$")
;;   (.getEval *sel* "alert(this)")
;;   (.getEval *sel* "this.browserbot.getCurrentWindow().__clj = function () {
;;     selenium.browserbot.getCurrentWindow().document.alert(this);
;;   };
;;   this.browserbot.getCurrentWindow().__clj();")

  (.isTextPresent *sel* "//a[contains(text(),'Logout')]")
  (.isElementPresent *sel* "//a[contains(text(),'Logout')]")
  (.waitForCondition *sel*
                     "dom=selenium.browserbot.getCurrentWindow().$"
                     "1000")
  (.getEval *sel* "dom=selenium.browserbot.getCurrentWindow().$x")
  (.getEval *sel* "dom=selenium.browserbot.getCurrentWindow().document.$x")
  (.stop *sel*)

  ;; java -jar /opt/algorithmics.com/algo-connect/selenium-server-1.0.1/selenium-server.jar  -firefoxProfileTemplate /Users/kburton/Library/Application\ Support/Firefox/Profiles/3o74fgym.Selenium1/

  (def *selenium* (new-selenium "phicarrot"))
  (.start *selenium*)
  ;; TODO: how to wait for the browser to start?
  (.open *selenium* "http://localhost/")

  (.isVisible *selenium* "xpath=//*[contains(text(),\"User\")]")
  (.isVisible *selenium* "xpath=//*[@value=\"Submit\"]")
  (.type *selenium* "xpath=//input[@id='login']" "admin")
  (.type *selenium* "xpath=//input[@id='password']" "monkey")
  (.submit *selenium* "xpath=//form")
  (.click *selenium* "xpath=//area")

  (.stop *selenium*)

  (sel-jquery *selenium* "$('div#nav').html()")

  (.getEval *selenium* "alert('foo')")
  (.getEval *selenium* "this.browserbot.getCurrentWindow().$")
  (.getEval *selenium* "this.browserbot.getCurrentWindow().jQuery('div#nav').html()")

  (.getEval *selenium* "this.browserbot.getCurrentWindow().jQuery('#margin_demands_unsent_table').html()")


  ;; how to write BDD / gherkin / Cucumber in clojure?
  (defcuke #"Then I click (\"[^\"]+\")" [tag-text]
    (click (format "xpath=//a[contains(text(),'%s')]" tag-text)))

  (cuke "Then I click \"Antic Demands\"")
  (cuke "Then I click \"Awaiting Demand Response\"")



;; .getValue <<locator>>
;; .type <<locator>> text
;; .submit <<form-locator>>


)
