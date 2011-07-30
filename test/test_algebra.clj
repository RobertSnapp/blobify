;;; test_algebra.clj is part of blobify.
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
;;; Tests the algebraic module of blobify, consisting of files:
;;; clj_ctree/vectors.clj
;;; clj_ctree/polynomial.clj

(ns test_algebra
  (:use blobify.vectors)
  (:use blobify.polynomial)
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
