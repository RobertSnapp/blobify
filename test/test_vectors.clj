;;; test_vectors.clj is part of blobify.
;;;
;;; blobify is a clojure program that indentifies and analyzes connected
;;; components in grayscale images and image stacks using component trees.
;;;
;;; Copyright (C) 2011 Robert R. Snapp
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;; Tests fundtions defined in namespace blobify.vectors

(ns test_vectors
  (:use blobify.utils
		blobify.vectors
		clojure.test))

(def eps10 1.0e-10)

(deftest inner-product-test
  (is (= (inner-product [1 2 3] [3 2 1]) 10))
  (is (= (inner-product [0 1 2] [0]) 0))
  (is (= (inner-product [] []) 0)))

(deftest vector-square-test
  (is (= (vector-square [0 0 0]) 0))
  (is (= (vector-square [1 -2 3 -4]) 30))
  (is (= (vector-square []) 0)))

(deftest vector-add-test
  (is (= (vector-add [0 1 2]) [0 1 2]))
  (is (= (vector-add [1 1 1 1] [2 2 2 2]) [3 3 3 3]))
  (is (= (vector-add [1 1] [2 2 2]) [3 3 2])))

(deftest vector-sub-test
  (is (= (vector-sub [1 2 3]) [1 2 3]))
  (is (= (vector-sub [1] [1]) [0]))
  (is (= (vector-sub [1 2 3] [1 2]) [0 0 3]))
  (is (= (vector-sub [10 10 10 10] [1 1 1 1] [2 2 2 2]) [7 7 7 7])))

(deftest vector-scale-test
  (is (= (vector-scale [1 2 3] 2) [2 4 6]))
  (is (= (vector-scale [1] 0) [0]))
  (is (= (vector-scale [16 -32] 4 -1/2 -1) [32 -64])))

(deftest vector-interp-test
  (is (= (vector-interp [0 1] [1 0] 1/2) [1/2 1/2]))
  (is (= (vector-interp [0] [1] 0) [0]))
  (is (= (vector-interp [0] [1] 1) [1]))
  (is (= (vector-interp [0] [1] 2) [2]))
  (is (< (l1-norm
		  (vector-sub (vector-interp [0.0] [1.0] 0.33)
					  [0.33])) eps10)))

(deftest basis-vector-test
  (is (= (basis-vector 2 3) [0 0 1])))

(deftest l-infinity-norm-test
  (is (= (l-infinity-norm [0 -1 2 -3 4]) 4))
  (is (= (sup-norm [4 -3 2 -1 0]) 4)))

(deftest l-infinity-distance-test
  (is (= (l-infinity-distance [0 1 -2] [5 6 7]) 9)))

(deftest l1-norm-test
  (is (= (l1-norm [1 -2 3]) 6))
  (is (= (l1-norm [-1 1 -1]) 3)))

(deftest l1-distance-test
  (is (= (l1-distance [0 3 2] [4 -1 6]) 12)))

(deftest lp-norm-test
  (is (absolutely-within eps10 (lp-norm 2 [3 4]) 5.0)))

(deftest lp-distance-test
  (is (absolutely-within eps10 (lp-distance 2 [5 5] [8 9]) 5.0)))

(deftest l2-norm-test
  (is (absolutely-within eps10 (l2-norm [3 4]) 5.0)))

(deftest l2-distance-test
  (is (absolutely-within eps10 (l2-distance [5 5] [8 9]) 5)))
