;; http://java.sun.com/docs/books/tutorial/uiswing/components/passwordfield.html
(ns com.github.kyleburton.sandbox.swing
  (:import (javax.swing JPanel JPasswordField JFrame JLabel JComponent
                        JButton SwingUtilities UIManager)
           (java.awt GridLayout FlowLayout)
           (java.awt.event ActionListener ActionEvent WindowAdapter)
           (java.util.concurrent CountDownLatch TimeUnit))
  (:require [com.github.kyleburton.sandbox.landmark-parser :as lparse]
            [com.github.kyleburton.sandbox.utils :as kutils])
  (:use [clojure.contrib.str-utils :as str]
        [clojure.contrib.fcase :only (case)]))

(defn get-password-dialog []
  (let [dlg-complete  (CountDownLatch. 1)
        frame         (JFrame. "Password")
        password      (atom nil)
        pass-field    (JPasswordField. 20)
        ok-txt        "Ok"
        cancel-txt    "Cancel"
        panel         (proxy
                          [JPanel ActionListener] 
                          []
                        (actionPerformed
                         [#^ActionEvent e]
                         (prn (format "actionPerformed: this=%s event=%s" this e))
                         (let [command (.getActionCommand e)
                               done (atom false)]
                           (prn (format "WindowAdapter.windowActivated, this=%s event=%s cmd=%s" this e command))
                           (if (= ok-txt command)
                             (do
                               (reset! password (.getPassword pass-field))
                               (reset! done true)))
                           (if (= cancel-txt command)
                             (do
                               (reset! done true)))
                           (if @done
                             (do
                               (.countDown dlg-complete)
                               (.setVisible frame false)
                               (.dispose frame))))))
        pass-label   (JLabel. "Password: ")
        button-panel (let [button-panel       (JPanel. (GridLayout. 0 1))
                           ok-button     (JButton. "OK")
                           cancel-button (JButton. cancel-txt)]
                       (.setActionCommand ok-button ok-txt)
                       (.addActionListener ok-button panel)
                       (.add button-panel ok-button)
                       (.setActionCommand cancel-button cancel-txt)
                       (.addActionListener cancel-button panel)
                       (.add button-panel cancel-button)
                       button-panel)
        text-pane    (JPanel. (FlowLayout. FlowLayout/TRAILING))]
    (SwingUtilities/invokeAndWait
     (proxy [Runnable] []
       (run []
            (UIManager/put "swing.boldMetal" Boolean/FALSE)
            ;; (.setDefaultCloseOperation frame  JFrame/EXIT_ON_CLOSE)
            (.setActionCommand  pass-field ok-txt)
            (.addActionListener pass-field panel)
            (.setLabelFor       pass-label pass-field)
            
            (.add text-pane pass-label)
            (.add text-pane pass-field)
            (.add panel text-pane)
            (.add panel button-panel)
            
            (.setOpaque panel true)
            (.setContentPane frame panel)
            (.addWindowListener
             frame
             (proxy [WindowAdapter] []
               (windowActivated 
                [e]
                (prn (format "WindowAdapter.windowActivated, this=%s event=%s" this e)))))
            (.pack frame)
            (.setVisible frame true))))
    (.await dlg-complete 5 TimeUnit/SECONDS)
    (prn (format "pass=%s" @password))
    (prn (format "pass=%s:%s" @password (if (nil? @password) "*null*" (String. @password))))
    @password))

;; (let [pass (get-password-dialog)] (prn "pass=" pass))