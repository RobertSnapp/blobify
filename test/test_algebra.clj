(ns test_algebra
  (:use snapp.algebra.vectors)
  (:use clojure.test))

(deftest inner-product-test
  (is (= (inner-product [1 2 3] [3 2 1]) 10))
  (is (= (inner-product [0 1 2] [0]) 0))
  (is (= (inner-product [] []) 0)))

(deftest vector-add-test
  (is (= (vector-add [0 1 2]) [0 1 2]))
  (is (= (vector-add [1 1 1 1] [2 2 2 2]) [3 3 3 3]))
  (is (= (vector-add [1 1] [2 2 2]) [3 3 2]))
  )

(deftest vector-scale-test
  (is (= (vector-scale 2 [1 2 3]) [2 4 6]))
  (is (= (vector-scale 0 [1]) [0])))

(deftest basis-vector-test
  (is (= (basis-vector 2 3) [0 0 1])))
