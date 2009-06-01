(ns com.github.kyleburton.sandbox.utils-test
  (:use [com.github.kyleburton.sandbox.utils :as kutils]
        [clojure.contrib.test-is]))

(deftest test-raise
  (kutils/assert-throws
   (/ 1 0)))

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
  (is (= "/foo" (kutils/basename "/foo/bar.txt")))
  (is (= nil (kutils/basename "bar.txt"))))

(deftest test-$HOME
  (is (= (str (get-user-home) "/foo.txt")
         (str (kutils/$HOME "foo.txt")))))

(deftest test-expand-file-name
  (is (= (str (kutils/$HOME "foo.txt"))
         (kutils/expand-file-name "~/foo.txt"))))

(deftest test-mkdir
  (is (kutils/mkdir "/"))
  (kutils/with-tmp-dir
   [tmp]
   (is (kutils/mkdir tmp))
   (is (kutils/mkdir tmp)))
  (kutils/with-tmp-file
   [tmp]
   (is (not (kutils/mkdir tmp)))))

