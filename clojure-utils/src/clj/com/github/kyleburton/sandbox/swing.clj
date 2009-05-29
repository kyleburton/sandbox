;; http://java.sun.com/docs/books/tutorial/uiswing/components/passwordfield.html
(ns com.github.kyleburton.sandbox.swing
  (:import (javax.swing JPanel JPasswordField JFrame JLabel JComponent
                        JButton SwingUtilities UIManager
                        JTable JScrollPane)
           (java.awt GridLayout FlowLayout GridBagLayout BorderLayout GridLayout)
           (java.awt.event ActionListener ActionEvent WindowAdapter)
           (java.util.concurrent CountDownLatch TimeUnit))
  (:require [com.github.kyleburton.sandbox.landmark-parser :as lparse]
            [com.github.kyleburton.sandbox.utils :as kutils]
            [com.github.kyleburton.sandbox.sql :as ksql])
  (:use [clojure.contrib.str-utils :as str]
        [clojure.contrib.fcase :only (case)]))

(def *ui-latch* (atom (CountDownLatch. 1)))

(defn ui-latch-reset! [& [count]]
  (reset! *ui-latch* (CountDownLatch. (or count 1))))

(defn ui-latch-relese! []
  (.countDown @*ui-latch*))

(def *current-ui* (atom nil))

(defn current-ui-set! [ui]
  (reset! *current-ui* ui))

(defn close-and-destroy-ui []
  (.setVisible @*current-ui* false)
  (.dispose @*current-ui*)
  (reset! *current-ui* nil)
  (ui-latch-relese!))

(defn get-password-dialog [& args]
  (ui-latch-reset!)
  (let [params        (kutils/parse-paired-arglist args)
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
                         (let [command (.getActionCommand e)
                               done (atom false)]
                           (prn (format "WindowAdapter.windowActivated, done=%s event=%s cmd=%s this=%s" 
                                        @done
                                        e
                                        command
                                        this))
                           (if (= ok-txt command)
                             (do
                               (reset! password (.getPassword pass-field))
                               (reset! done true)))
                           (if (= cancel-txt command)
                             (do
                               (reset! done true)))
                           (if @done
                             (do
                               (close-and-destroy-ui))))))
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
    (current-ui-set! frame)
    (SwingUtilities/invokeAndWait
     (proxy [Runnable] []
       (run []
            (UIManager/put "swing.boldMetal" Boolean/FALSE)
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
    (if (:timeout params)
      (.await @*ui-latch* (:timeout params) TimeUnit/SECONDS)
      (.await @*ui-latch*))
    @password))

;; (let [pass (get-password-dialog)] (prn (format "pass=%s/%s" (or pass "*null*") (String. (or pass "*null*")))))
;;
;; (close-and-destroy-ui)


(defn display-table-data [title cols data]
  (let [frame (JFrame. (str title))
        panel (JPanel.)
        table (JTable. (to-array-2d data)
                       (to-array cols))
        scroller (JScrollPane. table)]
    (.setResizingAllowed (.getTableHeader table) true)
    (.setAutoResizeMode table JTable/AUTO_RESIZE_SUBSEQUENT_COLUMNS)
    (.setLayout frame (GridLayout. 1 1))
    (.setContentPane frame panel)
    (.setLayout panel (BorderLayout. 2 1))
    (.add panel scroller)
    (.pack frame)
    (.setVisible frame true)
    frame))

(defn display-table-data-from-map [title data]
  (display-table-data title (keys (first data))
                      (map (fn [m]
                             (map #(% m) (keys m)))
                           data)))

(defn sql-browse-db [db]
  (display-table-data
   "DATABASE"
   ["SCHEMA_NAME"]
   (map (fn [x] [x]) (ksql/db-schemas db))))

;; TODO:  double-clicking on a name should either invoke sql-browse-table or sql-describe-table
(defn sql-browse-schema [db schema-name]
  (display-table-data
   schema-name
   ["TABLE_NAME" "SCHEMA" "TYPE" "CATALOG"]
   (map (fn [x] (map x [:name :schema :type :catalog])) (ksql/schema-tables db schema-name))))

(defn sql-browse-table [db table-name]
  (display-table-data 
   table-name
   (map :name (ksql/describe-table db table-name))
   (ksql/sql->records db (ksql/select-all table-name))))

(defn sql-describe-table [db table-name]
  (let [columns [:name :type :size :scale :is-nullable :precision]]
    (display-table-data
     table-name
     columns
     (map 
      (fn [x] (map x columns))
      (ksql/describe-table db table-name)))))
