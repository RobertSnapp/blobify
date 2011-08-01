;;; image.clj is part of blobify.
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
;;; This file defines a record for a two-dimensional raster image, called Image2d,
;;; a three-dimensionsal voxel image, called Image3d, and more general (and preferred)
;;; multidimensional image, called Image. All images are for the time being monochrome
;;; untagged, and uncompressed.
;;;
;;; Each structure is similar, consisting of a raster field that maintains the image
;;; data in a one-dimensional raster vector of integers. The other fields help manage the
;;; addressing, which is specialized to each type.
;;;
;;; Created by Robert R. Snapp, Copyright (c) 2010-2011.
;;;

(ns blobify.image
  (:import (java.awt.image BufferedImage DataBufferByte PixelGrabber WritableRaster)
           (java.awt Color Graphics Dimension)
           (javax.swing JPanel JFrame JLabel)
           (java.util.concurrent Executors)
           (ij ImagePlus)
           (ij.io Opener)
           (org.imagearchive.lsm.reader Reader))
  (:use 
        [blobify.polynomial :only (interpolate multi-horner)]
        [blobify.utils :only (dbg
                              debug
                              floor-rem
                              int-to-ubyte
                              ubyte-to-int
                              get-directory
                              get-filename
                              get-filename-ext
                              pos-if
                              seq2redundant-map)]
        [blobify.vectors :only (inner-product
                                l-infinity-distance
                                v+
                                v-
                                vector-divide
                                vector-square)]
        [clojure.contrib.pprint :only (cl-format)]
        [clojure.contrib.math :only (abs ceil)]
        [clojure.contrib.generic.math-functions :only (pow)]
        [clojure.contrib.combinatorics :only (cartesian-product)]))

#_(declare get-pixel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols

(defprotocol ImageAccess
  "Functions that facilite access to the content of a variety of records that
store different types of images defined using a finite-dimensional, rectangular
raster."
  (get-pixel [this offset] "Returns the image intensity at the indicated offset (arg2).")
  (get-size [this] "Returns the total number picture elements (pixels) in the image.")
  (get-dimensions [this] "Returns a vector indicating the maximum extent
along each coordinate, beginning with cols, rows, layers, frames, etc.")
  (get-dimensionality [this] "Returns the raster dimensionality.")
  (get-offset-of-site [this site] "Returns the raster offset of the indicated site vector.")
  (get-site-of-offset [this offset] "Returns the site vector of the indicated offset.")
  (scale-site [this site] "Returns the true physical coordinates of a raster site (e.g., microns.")
  )

;; Higher level functions based on ImageAccess
(defn get-pixel-site
  "Given an image (arg1) and a vector site (arg2) of the from [i0 ... in-1],
where n denotes the dimensionality of the image, (get-pixel-site image site)
returns the intensity of the indicated pixel."
  [image site]
  (get-pixel image (get-offset-of-site image site)))

(defn get-scale-factors
  "Returns the scale factor for each dimension."
  [image]
  (scale-site image (vec (take (get-dimensionality image) (repeat 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Image2d
;;
;; Image2d represents a two-dimensional raster image. 
;; The fields :rows and :cols indicate the integer-valued image dimesions.
;; The field :raster should indicate a one-dimensional vector that contains
;; the image raster.


(defrecord Image2d [cols rows raster])

(extend-type Image2d
  ImageAccess
  (get-pixel [{:keys [raster] :as this} offset]
             (ubyte-to-int (aget raster offset)))
  (get-size [{:keys [cols rows] :as this}]
            (* rows cols))
  (get-dimensions [{:keys [cols rows] :as this}]
                  (vector cols rows))
  (get-dimensionality [this]
                      2)
  (get-offset-of-site [{:keys [cols rows] :as this} site]
                      (+ (site 0) (* cols (site 1))))
  (get-site-of-offset [{:keys [cols rows] :as this} offset]
                      (let [r (int (/ offset cols))
                            c (rem offset cols)]
                        (vector c r)))
  (scale-site [this site] site)
  )

;;; Constructors
(defn make-image2d-from-fn
  [cols rows image-fn]
  (let [raster (byte-array (for [i (range rows) j (range cols)]
                             (int-to-ubyte (min 255 (image-fn j i)))))]
	(Image2d. cols rows raster)))


(defn make-image2d-pyramid
  [width]
  (make-image2d-from-fn
   width width
   (fn [j i] (- width (apply + (map #(abs (- % (/ width 2))) [j i]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Image3d represents a three-dimensional volumetric image.  
(defrecord Image3d [cols rows layers raster])

(extend-type Image3d
  ImageAccess
  (get-pixel [{:keys [raster] :as this} offset]
             (ubyte-to-int (aget raster offset)))
  (get-size [{:keys [cols rows layers] :as this}]
            (* rows cols layers))
  (get-dimensions [{:keys [cols rows layers] :as this}]
                  (vector cols rows layers))
  (get-dimensionality [this]
                      3)
  #_(get-offset-of-site [{:keys [cols rows] :as this} site]
                      (+ (site 0) (* cols (+ (site 1) (* rows (site 2))))))
  (get-offset-of-site [{:keys [n-cols n-rows] :as this} [c r l]]
                      (+ c (* n-cols (+ r (* n-rows l)))))
  (get-site-of-offset [{:keys [cols rows layers] :as this} offset]
                      (let [l (int (/ offset (* cols rows)))
                            r (int (/ (- offset (* l rows cols)) rows))
                            c (int (- offset (* rows (+ r (* cols l)))))]
                        (vector c r l)))
  (scale-site [this site] site)
  )

;; Constructors
(defn make-image3d-from-fn
  [cols rows layers image-fn]
   (let [raster (byte-array (for [k (range layers) i (range rows) j (range cols)]
					  (int-to-ubyte (min 255 (image-fn j i k)))))]
	(Image3d. cols rows layers raster)))

(defn make-image3d-pyramid
  [width]
  (make-image3d-from-fn
   width width width
   (fn [x y z] (- (* 3/2 width) (apply + (map #(abs (- % (/ width 2))) [x y z]))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Image represents a more general multi-dimensional image. The field indicated
;; by :dimensions should be a vector containing the image dimensions, and
;; :diimension-products should be a vector that contains successive products
;; of the image dimensions that is used for computing raster offset from
;; vector indicies. For example, a three-dimensional image, with dimensions
;; [d1, d2, d3] will have image offsets [1, d1, d1*d2]. This enables the
;; raster offest of voxel [i1, i2, i3] to be obtained by a simple inner-product:
;; i.e., (inner-product [i1, i2, i3] [1, d1, d1*d2]).

(defrecord Image [dimensions dimension-products raster])

;;; Utility functions to obtain raster offsets for the image structure

;;; We will frequently need to find the multi-dimensional site for
;;; a particular raster offset, and vice versa: the raster offset for a
;;; given site. The most efficient method uses a vector of
;;; dimension products, which can be precomputed, cached, and reused.

;;; dimension vector -> dimension-products vector:
(defn dimensions-to-dimension-products
  "Given a non-empty vector of dimensions that describe a multi-dimensional
  array [d1, d2, d3, ... dn], this function returns a vector containing the
  dimension products: [1, d1, d1*d2, d1*d2*d3, ..., d1*d2*d3*...*d([n-1]].
  The elements of this vector are useful for computing raster offsets
  in a multi-dimensional array. Note that the dimensionilty of the
  output is the same as the input, and an exception is called
  if the input is nil."
  [v]
  {:pre [(not (empty? v))]} ; ensure that v contains something.
  (loop [prods [1] last-product 1 dimensions (pop v)]
	(if (empty? dimensions)
	  prods
	  (let [next-product (* last-product (first dimensions))]
		(recur (conj prods next-product) next-product (rest dimensions))))))

;;; Conversion functions that require the provision of the dimension-products vector.
(defn with-dimension-products-site-to-offset
  "Given a vector of successive dimension products:
      [1, d1, d1*d2, ..., d1*d2*...*d[n-1]],
  and an index vector that identifies a particular location in a
  multidimensional array, returns the corresponding offset in a
  linear raster representation. Inverse to the function,
  with-dimension-products-offset-to-site."
  [dimension-products site]
  (inner-product dimension-products site))

(defn with-dimension-products-offset-to-site
  "Given a vector of succesive dimensions products:
      [1, d1, d1*d2, ..., d1*d2*...*d[n-1]],
   and an integer offset, this function returns the corresponding n-dimensional
   index vector [i1, i2, ..., in]. Inverse to the function,
   with-dimension-products-site-to-offset."
  [dimension-products offset]
  (loop [site [], balance offset, divisors dimension-products]
	(if (empty? divisors)
	  (vec site)
	  (let [current-divisor (peek divisors)]
		(recur (cons (int (/ balance current-divisor)) site)
			   (mod balance current-divisor) (pop divisors))))))

;;; Conversion functions that work directly from a vector that specifies
;;; each dimension of the image. Note that the with-dimension-products-*
;;; functions will be more efficient for repeated callls using the same
;;; image dimensions.

(defn with-dimensions-site-to-offset
  "Given a vector of array dimensions [d1, d2, d3, ..., dn], and an index vector
   [i1, i2, i3, ..., in], this function returns the offset of the indicated
   array element i1 + d1*i2 + d1*d2*i3 + ... + d1*d2*...*d(n-1)*in."
  [dimensions site]
  (multi-horner site (pop dimensions)))  ; defined in blobify.polynomial

(defn with-dimensions-offset-to-site
  "Given a vector of array dimensions [d1, d2, ..., dn] this function returns
   the multidimensional index [i1, i2, ..., in] of the indicated offset value."
  [dimensions offset]
  (let [dimension-products (dimensions-to-dimension-products dimensions)]
	(with-dimension-products-offset-to-site dimension-products offset)))

;;; Image creation
(defn make-image-from-fn
  "Creates a multidimensional image of the specified dimensions using a multidimensional
   function of corresponding dimensionality, image-fn, to set the value of each raster
   element. An image structure is returned."
  [dimensions image-fn]
  (let [dimension-products (dimensions-to-dimension-products dimensions)
		raster (byte-array (for [i (range (apply * dimensions))]
					  (apply (comp int-to-ubyte (partial min 255) image-fn)
							 (with-dimension-products-offset-to-site
							   dimension-products i))))]
	(Image. dimensions dimension-products raster)))

;;; Test multivariate fundtions.

(defn intensity-ramp
  "Defines a raster image that has a graduated intensity"
  [& indicies]
  (apply + indicies))

(defn checker-board
  "A function of a variable number of arguements that produces a checkerboard pattern consisting
   of cells that have sides of 4. Cells with odd parity index are set to 0, otherwise they are
   set to an amplitude between 0 and 255 according to the absolute value of the cosine of
   the product of the indicies."
  [& indicies]
  (let [ints (map #(int (/ % 3)) indicies),
		weights (cycle [0.7 0.39 0.17 0.29])]
	(if (even? (apply + ints))
	  (int (* 255 (Math/abs (Math/cos (inner-product weights ints)))))
	  0)))


;; a two-dimenions image with n (13) blobs
(defn make-blobby-image-2d
  "Generates a two-dimensional test image with width (arg1) pixels along each of
the two Cartesian directions. The image countains 13 positive clusters."
  [width]
  (let [n   13,						  ; number of intensity peaks
		tau 12,						  ; number of spiral revolutions in view
		sigma2*2 (* 0.0098 width width),
		a (/ width (* 4 Math/PI tau)),  ; sprial amplitude
		x0 (/ width 2),					; spiral center offset
		dTheta (/ (* 2 Math/PI tau) n),
		theta #(* 2 Math/PI),
		a*dTheta (* a dTheta),
		centers (map #(vector (+ x0 (* a*dTheta % (Math/cos (* dTheta %))))
							  (+ x0 (* a*dTheta % (Math/sin (* dTheta %))))) (range n)),
		amps (map #(/ 1000 (+ 10 %)) (range n)),
		gen   (fn [v]
				(int-to-ubyte (min 255 (inner-product amps
							   (map #(Math/exp
									  (/ (vector-square
										  (v- v %))
										 (- sigma2*2))) centers)))))
		dimensions (vector width width),
		dimension-products (dimensions-to-dimension-products dimensions)
		raster (byte-array (map #(gen
					  (with-dimension-products-offset-to-site
						dimension-products %))
					(range (apply * dimensions))))]
	(Image. dimensions dimension-products raster)))

;; a three-dimensions-image with n (13) blobs
(defn make-blobby-image-3d
  "Generates a three-dimensions test image with width (arg1) voxels along each of
the three Cartesian directions. The image contains 13 positive clusters."
  [width]
  (let [n   13,						  ; number of intensity peaks
		tau 12,						  ; number of spiral revolutions in view
		z_min (* 0.1 width),
		z_max (* 0.9 width),
		n_minus_1 (dec n)
		sigma2*2 (* 0.0098 width width),
		a (/ width (* 4 Math/PI tau)),  ; sprial amplitude
		x0 (/ width 2),					; spiral center offset
		dTheta (/ (* 2 Math/PI tau) n),
		theta #(* 2 Math/PI),
		a*dTheta (* a dTheta),
		centers (map #(vector (+ x0 (* a*dTheta % (Math/cos (* dTheta %))))
							  (+ x0 (* a*dTheta % (Math/sin (* dTheta %))))
							  (interpolate z_min z_max (/ % n_minus_1))
							  ) (range n)),
		amps (map #(/ 1500 (+ 10 %)) (range n)),
		gen   (fn [v]
				(int-to-ubyte (min 255 (inner-product amps
							   (map #(Math/exp
									  (/ (vector-square
										  (v- v %))
										 (- sigma2*2))) centers)))))
		dimensions (vector width width width),
		dimension-products (dimensions-to-dimension-products dimensions)
		size (apply * dimensions)
		raster (byte-array (map #(gen
					  (with-dimension-products-offset-to-site
						dimension-products %))
						 (range size)))]
	(Image. dimensions dimension-products raster)))



;;; Adjacency predicates

(defn lattice-neighbors?
  "Returns true if and only if the two vector arguments correspond to
   two neighboring sites in a lattice of arbitrary dimension. If only
   two arguments (the two site vectors) are provided, then a fully
   connected topology, in which each interior site has 3^n -1
   neighbors, is assumed. The optional third argument (threshold)
   facilitates more restrictive toplogies. For example, a threshold
   of 1 imposes the minimal nearest neighbor topoplogy with each site
   having 2n neighbors. A threshold of 2, adds 2n(n-1) next nearest
   neighbors, for a total of 2n^2. More generally, an integer
   threshold value of k defines a topology with
   2^k (n choose k) + 2^(k-1) (n choose k-1) + ... + 2^1 (n choose 1)
   neighbors."
  ([v1 v2 threshold]
	 {:pre [(< 0 threshold) (<= threshold (max (count v1) (count v2)))]}
	 (let [abs-diff (map #(Math/abs %) (seq (v- v1 v2)))]
	   (and 
		(== (apply max abs-diff) 1)		         ; l-infinity distance == 1, AND
		(<= (apply + abs-diff) threshold))))     ; l-one distance <= threshold
  ([v1 v2]
	 (== (l-infinity-distance v1 v2) 1)))        ; l-infinity distance == 1.

(defn with-dimension-products-neighbors?
  "A predicte that returns true of the two specified raster offsets correspond
   to neighboring sites (pixels, voxels, etc.) for a multidimensional image
   described by the given dimnensionality. An optional argument, threshold,
   enables the use of different lattice topologies.  See function
   lattice-neighbors?"
  [dimension-products offset1 offset2 & threshold]
  (let [v1 (with-dimension-products-offset-to-site dimension-products offset1)
        v2 (with-dimension-products-offset-to-site dimension-products offset2)]
    (if (empty? threshold)
      (lattice-neighbors? v1 v2)
      (lattice-neighbors? v1 v2 (first threshold)))))

(defn with-image-neighbors?
  "A predicte that returns true of the two specified raster offsets correspond
   to neighboring sites (pixels, voxels, etc.) for the given multidimensional image.
   An optional argument, threshold, enables the use of different lattice topologies.
   See function lattice-neighbors?"
   ([image offset1 offset2 threshold]
	  (with-dimension-products-neighbors? (:dimension-products image) offset1 offset2 threshold))
   ([image offset1 offset2]
	  (with-dimension-products-neighbors? (:dimension-products image) offset1 offset2 nil)))

;;; For morphological image analysis it is helpful to be able to construct a mask that can
;;; be used to define a topological neighborhood in a discrete lattice. For example, for
;;; a two-dimensional lattice,
;;;
;;;    ([0 1] [1 0] [0 -1] [-1 0])
;;;
;;; describes the neighborhood in a four-fold neigborhood (defined by a unit Hamming distance),
;;; while,
;;;
;;;   ([0 1] [1 0] [0 -1] [-1 1] [1 1] [1 -1] [-1 -1] [-1 0])
;;;
;;; does the same for an eight-fold neighborhood (defined by a Hamming distance of 2). The
;;; following functions generate generalizations of these neighborhoods to arbitrary lattice
;;; dimensions, and Hamming radii. The key function is lattice-offsets, defined below. Indeed
;;; the preceeding two lists are generated by (lattice-offsets 2 1) and (lattice-offsets 2 2)
;;; respectively.
;;;
;;; Each neighborhood is obtained by selecting the elements from a shift register sequence,
;;; generated using a ternary (m=3) de Bruijn cycle of order n, that fall within the
;;; indicated Hamming sphere.


(defn shift-register-seq
  "Given a pattern of symbols, such as a deBruijn (m=2, n=3) binary sequence [0 0 0 1 0 1 1 1],
this function generates k n-tuples that are obtained by sliding a window of size n, k times
along a sufficient number of cyclic repetitions of the pattern. Thus,
    (shift-register-seq 8 3 [0 0 0 1 0 1 1 1])
evaluates to
    ([0 0 0] [0 0 1] [0 1 0] [1 0 1] [0 1 1] [1 1 1] [1 1 0] [1 0 0])."
  [k n pattern]
  (map (fn [i] (vec (take n (drop i (cycle pattern))))) (range k)))

#_(def deBruijn5 '(0 0 0 0 0 1 0 0 0 1 1 0 0 1 0 1 0 0 1 1 1 0 1 0 1 1 0 1 1 1 1 1))
;; Evaluating (shift-register-seq 32 5 deBruijn5) generates a de Bruijn cycle for dim=5 bit numbers
;; as illustrated in D. E. Knuth, The Art of Computer Programing, vol. 4, Fascicle 2, p.22.

;; Donald E. Knuth, in The Art of Computer Programing, vol. 4, Fascicle 2, (2005), pp.22=28,
;; describes an algorithm by Harold Fredricksen and James Maiorana that generates all m-ary
;; prime sequences of length n or less in increasing order. Here Algorithm F is presented
;; in clojure:
(defn generate-preprimes
  "Given two integers, m and n, prime strings generates a vector of m-ary sequences of length n
that are preprime, that is are nonempty prefixes of a prime sequence. A sequence is prime if it
is lexocographically less than all of its proper suffixes. Thus 001122 is prime as it is less
than 01122, 1122, 122, 22, and 2."
  [m n]
  {:pre [(pos? m), (pos? n)]}
  (letfn [(get-submax-indices [a] (remove nil?
										  (map-indexed (fn [j x] (if (< x (dec m)) j)) a)))
		  (make-n-extension [x] (vec (take n (cycle x))))]
	(loop [a (vec (take n (repeat 0))),
		   preprimes (vector a)]
	  (let [indices (get-submax-indices a)]
		;; indices is a list of offsets of the components of a that are less than m - 1.
		(if (empty? indices)
		  preprimes
		  (let [j (last indices),
				;; the index j defines the offset of the rightmost component of a
				;; that is less than (dec m).
				a-next (make-n-extension (conj (vec (take j a)) (inc (a j))))]
			(recur a-next
				   (conj preprimes a-next))))))))

;; Knuth also shows how the generate-preprimes algorithm can be modified to generate
;; an m-ary de Bruijn cycle of order n.
(defn deBruijn-cycle
  "Generate an m-ary deBruijn cycle of order n."
  [m n]
  {:pre [(pos? m), (pos? n)]}
  (letfn [(divides-n? [x] (zero? (rem n x)))
		  (get-submax-indices [a] (remove nil?
										  (map-indexed (fn [j x] (if (< x (dec m)) j)) a)))
		  (make-n-extension [x] (vec (take n (cycle x))))]
	(loop [a (vec (take n (repeat 0))),
		   deBruijn (vector (vector 0))]
	  (let [indices (get-submax-indices a)]
		(if (empty? indices)
		  (apply concat deBruijn)
		  (let [j (last indices),
				a-next (make-n-extension (conj (vec (take j a)) (inc (a j))))]
			(recur a-next
				   (if (divides-n? (inc j))
					 (conj deBruijn (take (inc j) a-next))
					 deBruijn))))))))

#_(def deBruijn5 (deBruijn-cycle 2 5))

(defn get-neighborhood-mask
  "Generates a neighborhood mask, i.e., a list of sites that are adjacent to the
origin of an n-dimensional lattice with the provisio that they are within a
Hamming distance r from the origin."
  [n r]
  {:pre [(pos? n)]}
  (letfn [(hamming-norm [x] (apply + (map abs x)))]
	(let [dbc (map #(if (= % 2) -1 %) (deBruijn-cycle 3 n))]
	  (filter #(<= (hamming-norm %) r)
			  (drop 1 (shift-register-seq (int (pow 3 n)) n dbc))))))

(defn translate-mask
  "Translates a neighborhood mask so that it is centered on the indicated site."
  [mask site]
  (map #(v+ site %) mask))

(defn interior-site?
  "Returns true if and only if the indicated site falls within the n-dimensional
bounding box defined by the origin (inclusive) and the n-dimensional dimension
vector."
  [dimensions site]
  (every? true? (map (fn [d x] (and (<= 0 x) (< x d))) dimensions site)))

(defn get-neighboring-sites ;; old-name: translate-and-clip-neighborhood
  "Given a vector of image dimesnions (arg1) and a neighborhood mask, as might
be generated by get-neighborhood-mask (arg2), this function returns a list of
image sites that are adjacent to the indicated site (arg3), where the lattice
topology implicit in the neighborhood mask is assumed. (See also functions
get-neighboring-offsets, with-image-get-neighboring-offsets.)"
  [dimensions mask site]
  (filter (partial interior-site? dimensions) (translate-mask mask site)))

(defn get-neighboring-offsets
  "Given a vector of image dimensions (arg1), a vector of precomputed
dimension-products (arg2), and a neighborhood mask, as returned for example by
get-neighborhood (arg3), this function returns a list of raster offset addresses
that correspond to neighboring image sites of the offset value provided in
arg4. (See also with-image-generate-neighboring-offsets.)"
  [dimensions dimension-products mask offset]
  (let [site (with-dimension-products-offset-to-site dimension-products offset)]
	(map (partial with-dimension-products-site-to-offset dimension-products)
		 (get-neighboring-sites dimensions mask site))))

(defn with-image-get-neighboring-offsets
  "Given an image (arg1) and a neighborhood mask, as returned for example by
get-neighborhood (arg2), this function returns a list of raster offset addresses
that correspond to the neighboring image sites of the offest value provided in
arg3. (See also generate-neighboring-offsets.)"
  [image mask offset]
  (get-neighboring-offsets (:dimensions image)
						   (:dimension-products image) mask offset))

(defn get-filtered-neighborhood
  [image mask f offset]
  (filter (fn [x] (f (get-pixel image x)))
		  (with-image-get-neighboring-offsets image mask offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cartesian-slice
  "Given an image struct and a slice template (defined below),
   dimensions, this function returns the imaage2d struct that
   corresponds to the indicated slice.  The slice template
   consists of a vector with the same dimension as the image
   dimension. Exactly two components, should be specified as
   the symbol '*' which indicates don't care. The other components
   should evaluate to integers that fall within the admissible
   range of values for each dimension."
  [image]
  (let [filter (fn [i j] (get-pixel-site image (vector i j 3)))
		rows   ((:dimensions image) 0)
		cols   ((:dimensions image) 1)
		raster (for [i rows j cols]
				 (filter i j))]
	(Image2d. rows cols raster)))

(defn image-to-BuffferedImage
  "Creates a java BufferedImage object using the data from a two-dimensional
   slice that is defined by slice-fn, which maps two arguments to a
   multidimensional vector index. For example #([j i 3]) would project the
   slice with z=3, and #([j i i]) construct a diagonal slice."
  [image slice-fn cols rows]
  (let [buffer (BufferedImage. cols rows BufferedImage/TYPE_BYTE_GRAY)
		raster (.getRaster buffer)]
	(dorun
	 (for [i (range rows) j (range cols)]
	   (.setSample raster j i 0 (apply (partial get-pixel image) (slice-fn j i)))))
	buffer))

(defn identity-2d
  [j i]
  (vector j i))

(defn z-section
  [z]
  (fn [j i] (vector j i z)))

#_(defn xy-projection-of-3d
  [dimensions]
  (fn [j i] (apply + (map #() (range (peek dimensions))))))

#_(defn image2d-to-BufferedImage
  "Creates a test image of the indicated dimensions"
  [{:keys [rows cols] :as image2d}]
  (let [buffer (BufferedImage. cols rows BufferedImage/TYPE_BYTE_GRAY)
		raster (.getRaster buffer)]
	(dorun
	 (for [i (range rows) j (range cols)]
	   (.setSample raster j i 0 (get-pixel-site image2d (vector j i)))))
	buffer))

(defn grab-pixels
  "Returns an array containing the pixel values of a BufferedImage."
  [#^BufferedImage image]
  (let [w (.getWidth image)
        h (.getHeight image)
        pixels (make-array Integer/TYPE (* w h))]
    (doto (PixelGrabber. image 0 0 w h pixels 0 w)
      (.grabPixels))
    pixels))

(defn pprint-image-2d
   "Pretty prints the image intensities of the indicated image (arg1) as a two-dimensional array
of integers. Optional indices, for selecting a particular Cartesian slice if image has additional
dimensions, are collected into the list higher-indices, (arg2-argn)."
  [image & higher-indices]
  (let [[w h] (take 2 (get-dimensions image))
        d (get-dimensionality image)
        index-tail (take (max 0 (- d 2)) (concat higher-indices (repeatedly #(int 0))))]
    (dorun (for [i (range h)]
             (cl-format true  "~{~4a ~}~%"
                        (for [j (range w)]
                          (get-pixel-site image (vec (concat [j i] index-tail)))))))))

;; Obsolete
#_(defn pprint-image-2d
  "Pretty prints the image intensities of the indicated two-dimensional image as an array
of integers."
  [image]
  (let [w (nth (:dimensions image) 0)
        h (nth (:dimensions image) 1)]
	
	(dorun (for [i (range h)]
			 (cl-format true "~{~4a ~}~%"
                     (for [j (range w)]
                       (get-pixel image (with-image-site-to-offset image [j i]))))))
	nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  Public interface

(extend-type Image
  ImageAccess
  (get-pixel [{:keys [raster] :as this} offset]
             (ubyte-to-int (aget raster offset)))
  (get-size [{:keys [dimensions] :as this}]
            (apply * dimensions))
  (get-dimensions [{:keys [dimensions] :as this}]
                  dimensions)
  (get-dimensionality [{:keys [dimensions] :as this}]
                       (count dimensions))
  (get-offset-of-site [{:keys [dimension-products] :as this} site]
                      (with-dimension-products-site-to-offset dimension-products site))
  (get-site-of-offset [{:keys [dimension-products] :as this} offset]
                      (with-dimension-products-offset-to-site dimension-products offset))
  (scale-site [this site] site)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; StackedImage mantains multidimensional images as an array of rasters,
;;; each of the latter being a bytearray that represents a 2d image.
;;; The voxelSizne is a three-vector that describes the voxel dimensions
;;; in microns.

(defrecord StackedImage [path voxelSize dimensions dimension-products stack])

(debug :StackedImage)
(extend-type StackedImage
  ImageAccess
  (get-pixel [{:keys [dimension-products stack] :as this} offset]
             (let [[slice-index slice-offset] (floor-rem offset (nth dimension-products 2))]
               #_(dbg :StackedImage "slice-index = ~a, slice-offset = ~a~%" slice-index slice-offset)
               (int (bit-and (aget (nth stack slice-index) slice-offset) 0xff)))),
  
  (get-size [{:keys [dimensions] :as this}]
            (apply * dimensions))
  (get-dimensions [{:keys [dimensions] :as this}]
                  dimensions)
  (get-dimensionality [{:keys [dimensions] :as this}]
                       (count dimensions))
  (get-offset-of-site [{:keys [dimension-products] :as this} site]
                      (with-dimension-products-site-to-offset dimension-products site))
  (get-site-of-offset [{:keys [dimension-products] :as this} offset]
                      (with-dimension-products-offset-to-site dimension-products offset))
  (scale-site [{:keys [voxelSize] :as this} site] (vec (map * site voxelSize)))
)


(def m568 "/Users/snapp/data/cellNuclei/lsmData/m568/m568_1aaa.lsm")
(def m568c "/Users/snapp/data/cellNuclei/lsmData/m568/fiji/save/m568_1aaa_crop.tif")
(def m567 "/Users/snapp/data/cellNuclei/ctree/tif/m567_2kk.tif")
(def m588 "/Users/snapp/data/cellNuclei/ctree/tif/m588_1h.tif")

(defn openlsm
  "Open the lsm image indicated by path, and return the java ImagePlus object."
  [path]
  (let [reader (Reader.)
        verbose true
        thumbnail false
        directory (get-directory path)
        filename (get-filename path)
        imageplus (. reader open directory filename verbose thumbnail)
        height (. imageplus getHeight)
        width (. imageplus getWidth)
        size (.. imageplus getStack getSize)] ; number of images in the stack
    (cl-format true "Height=~a, Width=~a, StackSize=~a~%" height width size)
    imageplus))


;; Can we get the physical voxel dimensions?
#_(defn open-stack-image
  "Open the image stack indicated by path (usually a tif file), and return an StackedImage record."
  [path init-slice delta]
  (let [opener (Opener.)
        imageplus (. opener openImage path)
        height (. imageplus getHeight)
        width (. imageplus getWidth)
        cal (. imageplus getCalibration)
        voxelSize (vector (. cal pixelWidth) (. cal pixelHeight) (. cal pixelDepth))
        imageStack (. imageplus getImageStack)
        imageStackSize (. imageStack getSize)
        indices (range (inc init-slice) imageStackSize delta)
        dims [width height (count indices)]
        dp (dimensions-to-dimension-products dims)
        stack (vec (map #(.. imageStack  (getProcessor %) getPixels) indices))]
    (StackedImage. path voxelSize dims dp stack)))

(defn access-lsm-file
  "Returns an imageplus object that contains the image data referenced by the lsm file at path."
  [path]
  (let [reader (Reader.)
        thumbnail false
        verbose true
        directory (get-directory path)
        filename (get-filename path)]
    (. reader open directory filename verbose thumbnail)))

(defn access-tif-file
  "Returns an imageplus object that contains the image data referenced by the tif file at path."
  [path]
  (let [opener (Opener.)]
    (. opener openImage path)))

(defn open-stack-image
  "Open the image stack indicated by path (usually a tif or lsm file), and return an StackedImage record."
  [path init-slice delta]
  (let [imageplus (if (= (get-filename-ext path) ".lsm")
                    (access-lsm-file path)
                    (access-tif-file path))
        height (. imageplus getHeight)
        width (. imageplus getWidth)
        cal (. imageplus getCalibration)
        voxelSize (vector (. cal pixelWidth) (. cal pixelHeight) (. cal pixelDepth))
        imageStack (. imageplus getImageStack)
        imageStackSize (. imageStack getSize)
        indices (range (inc init-slice) imageStackSize delta)
        dims [width height (count indices)]
        dp (dimensions-to-dimension-products dims)
        stack (vec (map #(.. imageStack  (getProcessor %) getPixels) indices))
        ]
    (StackedImage. path voxelSize dims dp stack)))

;;;================
;;; Image Analysis
;;;================
;;; The following functions should work with every image type that implements the ImageAccess protocol.
;;; These functions extract quantitative information from an image.

(defn get-default-roi
  "Returns the default region of interest for the indicated image as a vector pair of offsets."
  [image]
  (vector 0 (get-size image)))

(defn get-default-roi-as-sites
  "Returns the default region of interest for the indicated image as a pair of lattice sites."
  [image]
  (map (partial get-site-of-offset image) (get-default-roi image)))

(defn get-default-roi-as-offset-delta-vector
  "Returns the default region of interest for the indicated image as a vector containing offset
values and deltas"
  [image]
  (let [roi-as-sites (get-default-roi-as-sites image)]
    (concat (first roi-as-sites) (v- (second roi-as-sites) (first roi-as-sites)))))

(defn get-max
  "Returns the maximum intensity in an image"
  [image]
  (apply max (map #(get-pixel image %) (range (get-size image)))))

(defn get-offsets
  "Returns a sequence of raster offsets that parameterize the specified image and region of interest (roi)."
  ([image]
     (range (get-size image)))
  ([image roi]
     (let [[min-site max-site] (map (partial get-site-of-offset image) roi)
           inc-max-site (map inc max-site)]
       (map (partial get-offset-of-site image)
         (apply cartesian-product (map range min-site inc-max-site))))))
  

(let [nthreads (.availableProcessors (Runtime/getRuntime))
      vals-per-thread 1024]
  (letfn [(serial-histogram [nbins vals]
            (loop [hist (vec (replicate nbins 0)) vals vals]
              (if (empty? vals)
                hist
                (let [x (max 0 (min (dec nbins) (int (first vals))))]
                  (recur (assoc hist x (inc (nth hist x)))
                         (rest vals))))))
          (parallel-histogram [nbins vals]
            (let [hist (vec (map ref (replicate nbins 0)))
                  pool (Executors/newFixedThreadPool nthreads)
                  tasks (map (fn [s]
                               (fn []
                                 (dorun
                                  (for [x s]
                                    (dosync
                                     (commute (hist (max 0 (min (dec nbins) x))) inc))))))
                             (partition-all (ceil (/ (count vals) vals-per-thread)) vals))]
              (doseq [future (.invokeAll pool tasks)]
                (.get future))
              (.shutdown pool)
              (map deref hist)))]
    (defn histogram
      "Creates a histogram of an image. With a single argument [image], a histogram of the entire image is
returned. With two arguments [image roi], where roi represents a 2-vector of raster offsets, then the
histogram is evaluated over the inclusive region of interest. "
      ([image]
         (serial-histogram 256 (pmap (partial get-pixel image) (get-offsets image))))
      ([image roi]
         (serial-histogram 256 (pmap (partial get-pixel image) (get-offsets image roi)))))))


(defn normalize
  "Normailizes a histogram (or other nonnegative vector) by dividing each entry by the sum."
  [hist]
  (let [sum (apply + hist)]
    (if (pos? sum)
      (vector-divide hist (float sum))
      hist)))

(defn successive-sums
  "Given a sequencs of numbers, computes the sequence of partial sums."
  ([nums]
     (successive-sums (first nums) (rest nums)))
  ([total nums]
     (if (empty? nums)
       (list total)
       (lazy-seq (cons total (successive-sums (+ total (first nums)) (rest nums)))))))

(defn cumulative-histogram
  ([image]
     (let [unh (successive-sums (histogram image))]
       (vector-divide unh (float (last unh)))))
  ([image roi]
     (let [unh (successive-sums (histogram image roi))]
       (vector-divide unh (float (last unh))))))

(defn inverse-cumulative-histogram
  "Inverse histogram accepts two arguments, a cumulative histogram ch (arg1) and a number from
the unit interval x (arg2). The index of the first entry that exceeds x is returned."
  [ch x]
  (pos-if #(>= % x) ch))

;;;;;
;;;;; Graphics
;;;;;

(defn render-image
  [{:keys [dimensions raster] :as image}]
  (let [frame (JFrame. "Image Viewer")
        cols (dimensions 0)
        rows (dimensions 1)
        max-side 1024
        scale (max (min (int (/ max-side (max cols rows))) 10) 1)
        buffer (new BufferedImage
                    (* scale cols)
                    (* scale rows)
                    BufferedImage/TYPE_INT_RGB)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g buffer 0 0 this)))
        graphics (.createGraphics buffer)]
    (doseq [x (range cols)
            y (range rows)]
      (let [v (get-pixel-site image (vector x y))]
        (doto graphics
          (.setColor (new Color v v v))
          (.fillRect (* x scale) (* y scale) scale scale))))
    (.add frame canvas)
    (.setSize frame (new Dimension (* scale rows) (* scale cols)))
    (.show frame)))

