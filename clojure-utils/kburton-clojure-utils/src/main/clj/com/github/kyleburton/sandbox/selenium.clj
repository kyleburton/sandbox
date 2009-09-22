(ns com.github.kyleburton.sandbox.selenium
  (:import  [com.thoughtworks.selenium DefaultSelenium])
  (:require [com.github.kyleburton.sandbox.log4j :as log]))


(def *log* (log/logger *ns*))

(def *selenium-defaults* (atom
                          {:host    "localhost"
                           :port    4444
                           :browser "*firefox"
                           :url     "http://localhost/"}))

(defn new-selenium [& [host port browser url]]
  (DefaultSelenium.
    (or host    (:host    @*selenium-defaults*))
    (or port    (:port    @*selenium-defaults*))
    (or browser (:browser @*selenium-defaults*))
    (or url     (:url     @*selenium-defaults*))))

(comment


  (def *selenium* (new-selenium))

  ;; java -jar /opt/algorithmics.com/algo-connect/selenium-server-1.0.1/selenium-server.jar  -firefoxProfileTemplate /Users/kburton/Library/Application\ Support/Firefox/Profiles/3o74fgym.Selenium1/

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
