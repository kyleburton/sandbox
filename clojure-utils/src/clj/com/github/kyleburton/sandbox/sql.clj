(ns com.github.kyleburton.sandbox.sql
  (use [clojure.contrib.sql :as sql]
       [com.github.kyleburton.sandbox.utils :as kutils]))


;; TODO: this apporoach only works if it's oracle
;; (with-db *db*
;;  (with-query-results rs ["SELECT * FROM ALL_TABLES"]
;;    (dorun
;;     (doseq [row rs]
;;       (prn (format "table: %s" (:table_name row)))))))

;; (with-db *db*
;;   (prn "conn: " (class (.getMetaData (connection)))))

;; (with-db *db*
;;   (prn "schemas: " (.getSchemas (.getMetaData (connection)))))


;; (defn table-names [db]
;;   (with-connection db
;;     (with-query-results rs ["SELECT TABLE_NAME FROM ALL_TABLES"]
;;       (doall (map :table_name rs)))))

;; (defn user-table-names [db]
;;   (with-connection db
;;     (with-query-results rs ["SELECT TABLE_NAME FROM USER_TABLES"]
;;       (doall (map :table_name rs)))))


;; (defn describe-table [db table-name]
;;   (with-connection db
;;       (with-query-results rs [(format "SELECT * FROM %s WHERE 0 = 1" table-name)]
;;         (prn (format "class of rs=%s/%s" rs (class rs))))))

;; (describe-table *db* "FOO")

;; (user-table-names *db*)

;; (filter #(.contains % "USER") (user-table-names *db*))

;; http://www.java2s.com/Code/Java/Database-SQL-JDBC/Listtablesinadatabase.htm
;; import java.sql.Connection;
;; import java.sql.DatabaseMetaData;
;; import java.sql.DriverManager;
;; import java.sql.ResultSet;
;; public class Main {
;;   public static void main (String args [])  throws Exception {
;;     Class.forName ("sun.jdbc.odbc.JdbcOdbcDriver");
;;     String URL = "jdbc:odbc:dbName";
;;     Connection conn = DriverManager.getConnection (URL, "user", "passw");
;;     DatabaseMetaData dmd = conn.getMetaData ();
;;     ResultSet rs1 = dmd.getSchemas ();
;;     while (rs1.next ())  {
;;       String ss = rs1.getString (1);
;;       ResultSet rs2 = dmd.getTables (null, ss, "%", null);
;;       while (rs2.next ())
;;         System.out.println (rs2.getString (3) + " " + rs2.getString (4));
;;     }
;;     conn.close ();
;;   }
;; }
