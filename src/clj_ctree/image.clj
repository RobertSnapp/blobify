;;; image.clj
;;;
;;; This file defines a structures for a two-dimensional raster image, called image2d,
;;; a three-dimensionsal voxel image, called image3d, and more general (and preferred)
;;; multidimensional image, called image. All images are for the time being monochrome
;;; untagged, and uncompressed.
;;;
;;; Each structure is similar, consisting of a raster field that maintains the image
;;; data in a one-dimensional raster vector of integers. The other fields help manage the
;;; addressing, which is specialized to each type.
;;;
;;; Created by Robert R. Snapp, Copyright (c) 2010.
;;;

(ns clj-ctree.image
  (:import (java.awt.image BufferedImage DataBufferByte PixelGrabber WritableRaster))
  (:use clj-ctree.vectors
		clj-ctree.polynomial
		clj-ctree.utils
		clojure.contrib.pprint
		[clojure.contrib.math :only (abs)]
		[clojure.contrib.generic.math-functions :only (pow)]))

(declare get-pixel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image2d
;;
;; image2d represents a two-dimensional raster image. 
;; The fields :rows and :cols indicate the integer-valued image dimesions.
;; The field :raster should indicate a one-dimensional vector that contains
;; the image raster.
(defstruct image2d :rows :cols :raster)

(defn make-image2d-from-fn
  [rows cols image-fn]
  (let [raster (vec (for [i (range rows) j (range cols)]
					  (image-fn j i)))]
	(struct image2d rows cols raster)))

(defn get-image2d-pixel
  "Return the value of the specified image pixel"
  [image i j]
  ((:raster image) (+ j (* i (:cols image)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image3d represents a three-dimensional volumetric image.  
(defstruct image3d :layers :rows :cols :raster)

(defn make-image3d-from-fn
  [layers rows cols image-fn]
   (let [raster (vec (for [k (range layers) i (range rows) j (range cols)]
					  (image-fn j i k)))]
	(struct image3d layers rows cols raster)))

(defn get-image3d-voxel
  "Return the value of the specified image voxel, situated in layer l, row r,
and column c."
  [image l r c]
  ((:raster image) (+ c (* (+  r (* l (:rows image))) (:cols image)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; image represents a more general multi-dimensional image. The field indicated
;; by :dimensions should be a vector containing the image dimensions, and
;; :diimension-products should be a vector that contains successive products
;; of the image dimensions that is used for computing raster offset from
;; vector indicies. For example, a three-dimensional image, with dimensions
;; [d1, d2, d3] will have image offsets [1, d1, d1*d2]. This enables the
;; raster offest of voxel [i1, i2, i3] to be obtained by a simple inner-product:
;; i.e., (inner-product [i1, i2, i3] [1, d1, d1*d2]).

(defstruct image :dimensions :dimension-products :raster)



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

;;; Obsolete. Replaced by site-to-offset using multi-horner
#_(defn with-dimensions-site-to-offset
  "Given a vector of array dimensions [d1, d2, d3, ..., dn], and an index vector
   [i1, i2, i3, ..., in], this function returns the offset of the indicated
   array element i1 + d1*i2 + d1*d2*i3 + ... + d1*d2*...*d(n-1)*in."
  [dimensions site]
  (let [dimension-products (dimensions-to-dimension-products dimensions)]
	(inner-product dimension-products site)))

(defn with-dimensions-site-to-offset
  "Given a vector of array dimensions [d1, d2, d3, ..., dn], and an index vector
   [i1, i2, i3, ..., in], this function returns the offset of the indicated
   array element i1 + d1*i2 + d1*d2*i3 + ... + d1*d2*...*d(n-1)*in."
  [dimensions site]
  (multi-horner site (pop dimensions)))  ; defined in clj-ctree.polynomial

(defn with-dimensions-offset-to-site
  "Given a vector of array dimensions [d1, d2, ..., dn] this function returns
   the multidimensional index [i1, i2, ..., in] of the indicated offset value."
  [dimensions offset]
  (let [dimension-products (dimensions-to-dimension-products dimensions)]
	(with-dimension-products-offset-to-site dimension-products offset)))


;;; Conversion functions that are sweetened to use an image, with a cached
;;; dimension-product vector, as a reference.
(defn with-image-site-to-offset
  "Given an image and an index vector, returns the corresponding scalar raster offset.
   Inverse of with-image-offset-to-image-vector"
  [image i-vec]
  (with-dimension-products-site-to-offset (:dimension-products image) i-vec))

(defn with-image-offset-to-site
  "Given an image and a raster offset, returns the corresponding index vector.
   Inverse of with-image-site-to-offset"
  [image offset]
  (with-dimension-products-offset-to-site (:dimension-products image) offset))


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
	(struct image dimensions dimension-products raster)))

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
										  (vector-sub v %))
										 (- sigma2*2))) centers)))))
		dimensions (vector width width),
		dimension-products (dimensions-to-dimension-products dimensions)
		raster (byte-array (map #(gen
					  (with-dimension-products-offset-to-site
						dimension-products %))
					(range (apply * dimensions))))]
	(struct image dimensions dimension-products raster)))

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
										  (vector-sub v %))
										 (- sigma2*2))) centers)))))
		dimensions (vector width width width),
		dimension-products (dimensions-to-dimension-products dimensions)
		size (apply * dimensions)
		raster (byte-array (map #(gen
					  (with-dimension-products-offset-to-site
						dimension-products %))
						 (range size)))]
	(struct image dimensions dimension-products raster)))



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
	 (let [abs-diff (map #(Math/abs %) (seq (vector-sub v1 v2)))]
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
   ([dimension-products offset1 offset2 threshold]
	 (let [v1 (with-dimension-products-offset-to-site dimension-products offset1)
		   v2 (with-dimension-products-offset-to-site dimension-products offset2)]
	   (if threshold
		 (lattice-neighbors? v1 v2 threshold)
		 (lattice-neighbors? v1 v2))))
   ([dimension-products offset1 offset2]
	  (with-dimension-products-neighbors? dimension-products offset1 offset2 nil)))

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
  (map #(vector-add site %) mask))

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
  (let [filter (fn [i j] (get-pixel image i j 3))
		rows   ((:dimensions image) 0)
		cols   ((:dimensions image) 1)
		raster (for [i rows j cols]
				 (filter i j))]
	(struct image2d rows cols raster)))


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


(defn image2d-to-BufferedImage
  "Creates a test image of the indicated dimensions"
  [{:keys [rows cols] :as image2d}]
  (let [buffer (BufferedImage. cols rows BufferedImage/TYPE_BYTE_GRAY)
		raster (.getRaster buffer)]
	(dorun
	 (for [i (range rows) j (range cols)]
	   (.setSample raster j i 0 (get-image2d-pixel image2d i j))))
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
  "Pretty prints the image intensities of the indicated two-dimensional image as an array
of integers."
  [image]
  (let [w (nth (:dimensions image) 0),
		h (nth (:dimensions image) 1)]
	
	(dorun (for [i (range h)]
			 (cl-format true "~{~4a ~}~%"
						(for [j (range w)]
						  (get-pixel image (with-image-site-to-offset image [j i]))))))
	nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  Public interface
(defn get-dimensionality
  "Given an image (arg1) returns the dimensionality of the image, usually 2 or 3."
  [image]
  (count (:dimensions image)))

(defn get-pixel
  "Given an image (arg1) and an address, either as a scalar offset (arg2), or as a
multi-dimensional site (args2 - argsN), get-pixel returns the scalar value of the
indicated pixel."
  ([{:keys [raster] :as image} offset]
	 (ubyte-to-int (aget raster offset)))
  ([{:keys [dimension-products raster] :as image} i1 i2 & irest]
	 (let [site (vec (concat (vector i1 i2) (vec irest)))]
	   (ubyte-to-int (aget raster (with-dimension-products-site-to-offset dimension-products site))))))

(defn get-size
  "Given an image (arg1), returns the number of components in the raster."
  [{:keys [dimensions] :as image}]
  (apply * dimensions))

(defn get-dimensions
  "Given an image (arg1) returns the dimensions of the image as a vector of ints."
  [{:keys [dimensions] :as image}]
  dimensions)
