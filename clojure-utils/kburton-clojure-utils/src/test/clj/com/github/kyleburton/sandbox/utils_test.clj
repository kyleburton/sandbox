(ns com.github.kyleburton.sandbox.utils-test
  (:use [com.github.kyleburton.sandbox.utils :as kutils]
        [clojure.test]))

(defmacro throws? [& body]
  `(let [did-throw# (atom false)]
     (try
      ~body
      (catch Exception ~'_ (reset! did-throw# true)))
     @did-throw#))

(deftest test-raise
  (is (throws? (/ 1 0)))
  '(is (not (throws? (/ 1 1)))))

(deftest test-get-user-home
  (is (= (System/getProperty "user.home")
         (kutils/get-user-home)))
  (is (= (str (kutils/$HOME))
         (kutils/get-user-home))))

(deftest test->file
  (is (= java.io.File
         (class (kutils/->file "foo"))))
  (is (= java.io.File
         (class (kutils/->file (java.io.File. "foo"))))))

(deftest test-basename
  (is (= "/foo" (kutils/basename (java.io.File. "/foo/bar.txt"))))
  (is (= "/foo" (kutils/basename "/foo/bar.txt")))
  (is (= nil    (kutils/basename "bar.txt"))))
;;(test-basename)

(deftest test-$HOME
  (is (= (str (get-user-home) "/foo.txt")
         (str (kutils/$HOME "foo.txt")))))


(deftest test-expand-file-name
  (is (= (str (kutils/$HOME "foo.txt"))
         (kutils/expand-file-name "~/foo.txt")))
  (is (= (format "file://%s/foo.txt" (System/getProperty "user.home"))
         (kutils/expand-file-name "file://~/foo.txt"))))

(deftest test-mkdir
  (is (kutils/mkdir "/"))
  (kutils/with-tmp-dir
   [tmp]
   (is (kutils/mkdir tmp))
   (is (kutils/mkdir tmp)))
  (kutils/with-tmp-file
   [tmp]
   (is (not (kutils/mkdir tmp)))))


;; (deftest test-doc-class
;;   (with-open [out (java.io.StringWriter.)]
;;     (binding [*out* out]
;;       (is (not (doc-class "")))
;;       (is (not (doc-class String))))))

(deftest test-exec
  (is "foo" (exec "echo foo")))

(deftest obj-to-file-and-back
  (with-tmp-file [tmp]
    (let [dat "foo"]
      (object->file dat tmp)
      (is (= dat (file->object tmp))))))


(deftest test-freeze-thaw
  (let [dat {:a 1 :b 2}
        ser (apply freeze dat)
        ret (thaw ser)]
    (is (= (:a dat)
           (:a ret)))
    (is (= (:b dat)
           (:b ret)))))

;; (test-freeze-thaw)
;; (thaw (freeze {:a 1 :b 2}))

(deftest test-make-subst-fn
  (let [f (make-subst-fn " " "&nbsp;")]
    (is (= "&nbsp;" (f " ")))
    (is (= "x" (f "x")))))

(deftest test-md5->string
  (is (< 0 (count (md5->string (freeze "foo"))))))

(deftest test-sha1->string
  (is (< 0 (count (sha1->string (freeze "foo"))))))

(deftest test-pairs->map
  (let [m (pairs->map [ :a 1 :b 2])]
    (is (= 1 (:a m)))
    (is (= 2 (:b m))))
  (is (= 0 (count (pairs->map [])))))



