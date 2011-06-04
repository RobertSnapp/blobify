;;; Copyright 2011 Robert R. Snapp
;;;
;;; This file is part of clj-ctree.
;;;
;;; clj-ctree is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published
;;; by the Free Software Foundation, either version 3 of the License,
;;; or (at your option) any later version.
;;;
;;; clj-ctree is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with clj-ctree.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;-------------------------------------------------------------------
;;;
;;; This namespace provides vector and matrix utility functions used
;;; by other parts of clj-ctree.


(ns clj-ctree.vectors)

;;; Data representation.
;;;
;;; An n-dimensional vector is represented simply as a clojure vector, for
;;; example the basis vectors in 3-space, are [1 0 0], [0 1 0], and [0 0 1].
;;;
;;; A general matrix is represented as a vector of row vectors. Thus if a23 represents
;;; the element in the third column of the second row of matrix a, then matrix
;;; a is represented as
;;;
;;;   [[a11 a12 a13]
;;;    [a21 a22 a23]
;;;    [a31 a32 a33]]
;;;
;;; A symmetric matrix is represted in upper triangular form
;;;
;;;   [[a11 a12 a13]
;;;    [a22 a23]
;;;    [a33]]
;;;
;;; ------------------------------------------------------------

#_(defrecord Matrix [dims data])
#_(defrecord SymmetricMatrix [dims data])

#_(defprotocol MatrixAccess
  (getValue [this i j] "Returns the value of the element in row i, column j."))


(defn inner-product
  "Computes the inner product of two vectors"
  [x y]
  (apply + (map #(* %1 %2) x y)))

(defn vector-square
  "Computes the square of a vector"
  [x]
  (inner-product x x))

(defn vector-add
  "Adds two or more vectors"
  ([v1] v1)
  ([v1 v2]
  (let [sum (map #(+ %1 %2) v1 v2)
		v1c (count v1)
		v2c (count v2)]
	(if (< v1c v2c)
	  (vec (concat sum (drop v1c v2)))
	  (vec (concat sum (drop v2c v1))))))
  ([v1 v2 & more] (reduce vector-add (vector-add v1 v2) more)))

(defn vector-sub
  "Subtract two or more vectors"
  ([v1] v1)
  ([v1 v2]
  (let [diff (map #(- %1 %2) v1 v2)
		v1c (count v1)
		v2c (count v2)]
	(if (< v1c v2c)
	  (vec (concat diff (map #(- %) (drop v1c v2))))
	  (vec (concat diff (drop v2c v1))))))
  ([v1 v2 & more] (reduce vector-sub (vector-sub v1 v2) more)))

(defn vector-divide
  "Divides a every element in vector v by a denominator d"
  [v d]
  (vec (map #(/ % d) v)))

(defn vector-multiply
  "Multiplies a every element in vector v by a scalar s"
  [v s]
  (vec (map #(* % s) v)))

(defn vector-interp
  "Returns a vector that is the linear interpolation between vectors v0 (arg1) and v1 (arg2),
   with repsect to x (arg3), is that v0 is returned if f x=0, and v1 is returned if x=1."
  [v0 v1 x]
  (vector-add (vector-multiply v0 (- 1 x)) (vector-multiply v1 x)))

(defn basis-vector
  "Generates an n-dimensional Cartesian basis vector conisting
   of all elements zero, except for that with index i, which
   is set to 1."
  [i n]
  {:pre [(<= 0 i) (< i n)]}
  (vec (map #(if (= % i) 1 0) (range n))))

(defn l-infinity-norm
  "Retruns the l-infinity-norm, or sup-norm, of the input vector v."
  [v]
  (apply max (map #(Math/abs %) (seq v))))

(def sup-norm l-infinity-norm)

(defn l-infinity-distance
  [v1 v2]
  (l-infinity-norm (vector-sub v1 v2)))

(defn l1-norm
  [v]
  (apply + (map #(Math/abs %) (seq v))))


(defn l1-distance
  [v1 v2]
  (l1-norm (vector-sub v1 v2)))

(defn l2-norm
  "Returns the L2, or Euclidean, norm of vector v."
  [v]
  (Math/sqrt (vector-square v)))


(defn l2-distance
  "Returns the L2 or Euclidean distance between vectors v1 and v2."
  [v1 v2]
  (l2-norm (vector-sub v1 v2)))


(defn lp-norm
  "Returns the Lp norm of vector v."
  [p v]
  (Math/pow (apply + (map #(Math/pow (Math/abs %) p) (seq v))) (/ p)))

(defn lp-distance
  "Returns the Lp distance between vectors v1 and v2."
  [p v1 v2]
  (lp-norm p (vector-sub v1 v2)))


(defn flat-vector-auto-product
  "Generates an upper-triangular matrix that contains the elements of outer product
of vector v (arg1) with itself. This function is useful for computing mixed second
moments and covariance matrics. By 'flat' we mean that the upper-triangluar matrix
is represented as a single vector of dimension n(n+1)/2, where n is the dimension of v."
  [v]
  (vec (apply concat (map #(for [x %2] (* %1 x)) v (take (count v) (iterate rest v))))))
