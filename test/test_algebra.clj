;;; test_algebra.clj
;;;
;;; Tests the algebraic module of clj-ctree, consisting of files:
;;; clj_ctree/vectors.clj
;;; clj_ctree/polynomial.clj

(ns test_algebra
  (:use clj-ctree.vectors)
  (:use clj-ctree.polynomial)
  (:use clojure.test))



;;; Testing polynomial functions
(deftest horner-test
  (is (= (horner [] 1) 0))
  (is (= (horner [1 2 3 4 5] 2)) 209)
  (is (= (horner [1 1 1 1] -1) 0)))

(deftest multi-horner-test
  (is (= (multi-horner [1] []) 1))
  (is (= (multi-horner [1 2] [1]) 3))
  (is (= (multi-horner [1 2 3 4] [1 2 3]) 33)))
