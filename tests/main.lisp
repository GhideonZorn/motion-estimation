(defpackage TP1/tests/main
  (:use :cl
        :TP1
        :rove))
(in-package :TP1/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :TP1)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
