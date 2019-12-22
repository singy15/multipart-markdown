(defpackage multipart-markdown/tests/main
  (:use :cl
        :multipart-markdown
        :rove))
(in-package :multipart-markdown/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :multipart-markdown)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
