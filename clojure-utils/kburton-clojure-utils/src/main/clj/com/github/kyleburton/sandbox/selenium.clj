(ns com.github.kyleburton.sandbox.selenium
  (:import  [com.thoughtworks.selenium DefaultSelenium])
  (:require [com.github.kyleburton.sandbox.log4j :as log]
            [com.github.kyleburton.sandbox.swing :as gui]))

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
  (not (= "null" (.getEval sel "dom=selenium.browserbot.getCurrentWindow().Ajax"))))

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

(def *password* (atom nil))
(defn get-password []
  (if @*password*
    @*password*
    (reset! *password* (String. (gui/get-password-dialog)))))

(defn selenium-js-setup [sel]
  (.getEval sel "selenium.browserbot.getUserWindow().document.doEval = function(script) {
    try {
        return eval(
           \"var document = selenium.browserbot.getUserWindow().document;\\n\" +
           \"var window = selenium.browserbot.getUserWindow();\\n\" +
           script);
    } catch (e) {
        throw new SeleniumError(\"Threw an exception: \" + e.message);
    }
};"))


(defn eval-js [sel js]
  (selenium-js-setup sel)
  (.getEval sel
            (format "selenium.browserbot.getUserWindow().document.doEval('%s')"
                    (.replaceAll
                     (.replaceAll js "'" "\\\\'")
                     "\n" "\\\\n"))))

(comment

  (def *sel* (new-selenium))
  (.start *sel*)
  (.stop *sel*)

  (selenium-setup *sel*)
  (eval-js *sel* "document.location='http://google.com/';")
  (eval-js *sel* "document.location='http://yahoo.com/';")
  (eval-js *sel* "window.document")

  (eval-js *sel* "document.evaluate('//a',document,null, XPathResult.ANY_TYPE, null).iterateNext()")
  ;; TODO: move this into the selenium-setup so it's always available...
  (eval-js *sel* "doXpath = function(xpath) {
    var results = [], xpathResult = document.evaluate(xpath,document,null, XPathResult.ANY_TYPE, null);
    for ( var itr = xpathResult.iterateNext(); itr; itr = xpathResult.iterateNext() ) {
      results.push(itr);
    }
    return results;
  };")
  (eval-js *sel* "doXpath('//a')[0].nodeName")
  (eval-js *sel* "doXpath('//a')[0].nodeValue")
  (eval-js *sel* "doXpath('//a')[1]")
  (eval-js *sel* "doXpath('//a')")
  (eval-js *sel* "$(doXpath('//a')[0]).innerHTML")
  (eval-js *sel* "$(doXpath('//a')[0]).innerHTML = '123'")


  (.open *sel* "http://localhost/")
  ;; neither 'this.browserbot.getCurrentWindow()', nor 'selenium.browserbot.getCurrentWindow()' seems
  ;; to get us to the application's window...
  ;;(eval-js *sel* "document.write('foof')")
  (sel-input *sel* "login" "admin")

  (sel-input *sel* "password" (get-password))
  (sel-submit *sel* "//form")
  (sel-click *sel* "//area")
  (sel-click *sel* "//a[contains(text(),'Logout')]")

  ;;   /html/body/div/div[3]/div/map/area
  ;; clickable things are: a'nchor tags, map/area's and anything with an onClick handler...

  ;(.start *sel*)
  ;; selenium.waitForCondition("selenium.browserbot.getCurrentWindow().Ajax.activeRequestCount == 0;", DEFAULT_WAIT_PERIOD);

  ;; jQuery:    jQuery.active
  ;; Prototype: Ajax.activeRequestCount
  (.getEval *sel* "this.browserbot.getCurrentWindow().jQuery.active")
  (.open *sel* "http://localhost/")

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
    (type! "password" (get-password))
    (submit! "//form")
    (.waitForPageToLoad *sel* "500")
    (click! "//area")
  )

  (jquery? *sel*)
  (prototype? *sel*)

  (.isTextPresent *sel* "//a[contains(text(),'Logout')]")
  (.isElementPresent *sel* "//a[contains(text(),'Logout')]")
  (.waitForCondition *sel*
                     "dom=selenium.browserbot.getCurrentWindow().$"
                     "1000")
  (.getEval *sel* "dom=selenium.browserbot.getCurrentWindow().$x")
  (.getEval *sel* "dom=selenium.browserbot.getCurrentWindow().document.$x")
  (.stop *sel*)

  (.start *selenium*)
  ;; TODO: how to wait for the browser to start?
  (.open *selenium* "http://localhost/")

  (.isVisible *selenium* "xpath=//*[contains(text(),\"User\")]")
  (.isVisible *selenium* "xpath=//*[@value=\"Submit\"]")
  (.type *selenium* "xpath=//input[@id='login']" "admin")
  (.type *selenium* "xpath=//input[@id='password']" (get-password))
  (.submit *selenium* "xpath=//form")
  (.click *selenium* "xpath=//area")

  (.stop *selenium*)

  (sel-jquery *selenium* "$('div#nav').html()")


)
