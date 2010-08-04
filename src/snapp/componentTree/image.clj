;;; image.clj
;;;
;;; This file defines a structure for a two-dimensional image, called image.
;;; Create a simple test image.

(ns snapp.componentTree.image
  (:import (java.awt.image BufferedImage PixelGrabber WritableRaster ))
  (:use snapp.algebra.vectors))

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


;; image3d represents a three-dimensional volumetric image.  
(defstruct image3d :layers :rows :cols :raster)

(defn make-image3d-from-fn
  [layers rows cols image-fn]
   (let [raster (vec (for [k (range layers) i (range rows) j (range cols)]
					  (image-fn j i k)))]
	(struct image3d layers rows cols raster)))

(defn get-image3d-voxel
  "Return the value of the specified image voxel, situated in layer l, row r, and column c."
  [image l r cj]
  ((:raster image) (+ c (* (+  r (* l (:rows image))) (:cols image)))))

;; image represents a more general n-dimensional image. The field indicated by
;; :dimensions should be a vector containing the image dimensions.
;; Thus it assumes the form [rows cols] for a 2-d image, and [rows cols layers]
;; for a 3-d image, etc.
(defstruct image :dimensions :raster)

;;; Utility functions to obtain raster offsets for the image structure

(defn dimensions-to-offset-vector
  "Given a vector of dimensions that describe a multi-dimensional array
  [d1, d2, d3, ... dn], this function returns a vector containing the
  offset-vector [d1, d1*d2, d1*d2*d3, ..., d1*d2*d3*...*dn]."
  [v]
  (loop [prods [] last-product 1 dimensions v]
	(if (empty? dimensions)
	  prods
	  (let [next-product (* last-product (first dimensions))]
				 (recur (conj prods next-product) next-product (rest dimensions))))))

;; this should be implemented using Horner's rule!
(defn index-vector-to-offset
  "Given a vector of array dimensions [d1, d2, d3, ..., dn], and an index vector
   [i1, i2, i3, ..., in], this function returns the offset of the indicated
   array element i1 + d1*i2 + d1*d2*i3 + ... + d1*d2*...*d(n-1)*in."
  [dimensions index-vector]
  
  (let [dimension-products (cons 1 (pop (dimensions-to-offset-vector dimensions)))]
	(inner-product dimension-products index-vector)))

(defn offset-to-index-vector
  "Given a vector of array dimensions [d1, d2, ..., dn] this function returns
   the multidimensional index [i1, i2, ..., in] of the indicated offset value."
  [dimensions offset]
  (let [dimension-products (pop (dimensions-to-offset-vector dimensions))]
	(loop [index-vector [], balance offset, divisors dimension-products]
	  (if (empty? divisors)
		(vec (cons balance index-vector))
		(let [current-divisor (peek divisors)]
		  (recur (cons (int (/ balance current-divisor)) index-vector)
				 (mod balance current-divisor) (pop divisors)))))))
  
(defn get-image-element
  "Returns the image element stored at the indicated index vector. An exception occurs
   if the dimensionality of the index vector disagrees with that of the image."
  [image index-vector]
  ((:raster image) (index-vector-to-offset (:dimensions image) index-vector)))

(defn make-image-from-fn
  "Creates a test image"
  [dimensions image-fn]
  (let [raster (vec (for [i (range (apply * dimensions))] (apply image-fn (offset-to-index-vector dimensions i))))]
	(struct image dimensions raster)))


(defn get-image-slice
  "Creates a java BufferedImage object using the data from a two-dimensional
   slice that is defined by slice-fn, which maps two arguments to a
   multidimensional vector index. For example #([j i 3]) would project the
   slice with z=3, and #([j i i]) construct a diagonal slice."
  [image slice-fn cols rows]
  (let [buffer (BufferedImage. cols rows BufferedImage/TYPE_BYTE_GRAY)
		raster (.getRaster buffer)]
	(dorun
	 (for [i (range rows) j (range cols)]
	   (.setSample raster j i 0 (get-image-element image (slice-fn j i)))))
	buffer))



(defn make-image-buffer
  "Creates a test image of the indicated dimensions"
  [{:keys [rows cols] :as image2d}]
  (let [buffer (BufferedImage. cols rows BufferedImage/TYPE_BYTE_GRAY)
		raster (.getRaster buffer)]
	(dorun
	 (for [i (range rows) j (range cols)]
	   (.setSample raster j i 0 (get-image2d-pixel image2d i j))))
	buffer))

(defn intensity-ramp
  "Defines a raster image that has a graduated intensity"
  [i j]
  (+ i j))

(defn grab-pixels
  "Returns an array containing the pixel values of a BufferedImage."
  [#^BufferedImage image]
  (let [w (.getWidth image)
        h (.getHeight image)
        pixels (make-array Integer/TYPE (* w h))]
    (doto (PixelGrabber. image 0 0 w h pixels 0 w)
      (.grabPixels))
    pixels))



