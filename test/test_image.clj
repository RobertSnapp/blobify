;;; test_image.clj is part of blobify.
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

(ns test_image
  (:use blobify.image
		clojure.test))

(deftest image2d_test
  (let [im (make-image2d-from-fn 32 16 checker-board)]
	(is (= (:cols im) 32))
	(is (= (:rows im) 16))
	(is (= (count (:raster im)) (* (:rows im) (:cols im))))
	(is (= (get-pixel-site im [0 0]) 255))))

(deftest image3d_test
  (let [im (make-image3d-from-fn 32 16 8 checker-board)]
	(is (= (:cols im) 32))
	(is (= (:rows im) 16))
	(is (= (:layers im) 8))
	(is (= (count (:raster im)) (* (:layers im) (:rows im) (:cols im))))
	(is (= (get-pixel-site im [0 0 0]) 255))))

(deftest image_offset_conversion
  (let [ivec [1 2 3]
		dvec [10 20 30]
		dprods (dimensions-to-dimension-products dvec)
		offset 621]
	(is (= (with-dimensions-site-to-offset dvec ivec) offset))
	(is (= (with-dimensions-offset-to-site dvec offset) ivec))
	(is (= (with-dimension-products-site-to-offset dprods ivec) offset))
	(is (= (with-dimension-products-offset-to-site dprods offset) ivec))))

(deftest image_test
  (let [dvec [10 20 30],
		dprods (dimensions-to-dimension-products dvec),
		img (make-image-from-fn
			 dvec
			 #(mod (with-dimensions-site-to-offset dvec [%1 %2 %3]) 255)),
		iters 10]
	(is (= (:dimension-products img) dprods))
	(dotimes [i iters]
	  (let [ivec (map rand-int dvec)]
       (is (= (get-pixel-site img ivec)
             (mod (get-offset-of-site img ivec) 255)))))))


(deftest lattice-neighbor-test
  (is (lattice-neighbors? [0 0 0] [1 0 1]))
  (is (lattice-neighbors? [0 0 0] [0 0 1] 1))
  (is (not (lattice-neighbors? [0 0 0] [1 1 1] 1)))
  (let [dprods (dimensions-to-dimension-products [10 10 10])]
	(is (with-dimension-products-neighbors?
		  dprods
		  (with-dimension-products-site-to-offset dprods [5 5 5])
		  (with-dimension-products-site-to-offset dprods [5 5 6])
		  1))
	(is (not (with-dimension-products-neighbors?
			   dprods
			   (with-dimension-products-site-to-offset dprods [5 5 5])
			   (with-dimension-products-site-to-offset dprods [6 6 6])
			   1))))
  (let [img-2d (make-blobby-image-2d 100)]
;;	(is (= (apply * (:dimensions img-2d)) (count (:raster img-2d))))
	(is (= ((:dimension-products img-2d) 0) 1))
	(is (= ((:dimension-products img-2d) 1) ((:dimensions img-2d) 0) 100))
	(is (with-image-neighbors?
		  img-2d
		  (get-offset-of-site img-2d [50 50])
		  (get-offset-of-site img-2d [51 50])
		  1))
	(is (not (with-image-neighbors?
			   img-2d
			   (get-offset-of-site img-2d [50 50])
			   (get-offset-of-site img-2d [51 51])
			   1))))
   (let [img-3d (make-blobby-image-3d 10)]
;;	(is (= (apply * (:dimensions img-3d)) (count (:raster img-3d))))
	(is (= ((:dimension-products img-3d) 0) 1))
	(is (= ((:dimension-products img-3d) 1) ((:dimensions img-3d) 0) 10))
	(is (= ((:dimension-products img-3d) 2)
		   (apply * (map #((:dimensions img-3d) %) [0 1]))
		   100))
	(is (with-image-neighbors?
		  img-3d
		  (get-offset-of-site img-3d [5 5 5])
		  (get-offset-of-site img-3d [5 6 5])
		  1))
	(is (not (with-image-neighbors?
			   img-3d
			   (get-offset-of-site img-3d [5 5 5])
			   (get-offset-of-site img-3d [6 5 6])
			   1)))))

(deftest shift-register-seq-test
  (let [seq-k8-n3-p00010111 '([0 0 0] [0 0 1] [0 1 0] [1 0 1] [0 1 1] [1 1 1] [1 1 0] [1 0 0])]
	(is (= (shift-register-seq 8 3 [0 0 0 1 0 1 1 1]) seq-k8-n3-p00010111))))

(deftest generate-preprimes-test
  (let [preprimes-m3-n4 [[0 0 0 0] [0 0 0 1] [0 0 0 2] [0 0 1 0] [0 0 1 1] [0 0 1 2] [0 0 2 0] [0 0 2 1]
						 [0 0 2 2] [0 1 0 1] [0 1 0 2] [0 1 1 0] [0 1 1 1] [0 1 1 2] [0 1 2 0] [0 1 2 1]
						 [0 1 2 2] [0 2 0 2] [0 2 1 0] [0 2 1 1] [0 2 1 2] [0 2 2 0] [0 2 2 1] [0 2 2 2]
						 [1 1 1 1] [1 1 1 2] [1 1 2 1] [1 1 2 2] [1 2 1 2] [1 2 2 1] [1 2 2 2] [2 2 2 2]]]
	(is (= (generate-preprimes 3 4) preprimes-m3-n4))))

(deftest deBruijn-cycle-test
  (let [cycle-m2-n5 '(0 0 0 0 0 1 0 0 0 1 1 0 0 1 0 1 0 0 1 1 1 0 1 0 1 1 0 1 1 1 1 1)]
	(is (= (deBruijn-cycle 2 5) cycle-m2-n5))))

(deftest get-neighborhood-mask-test
  (let [offsets-n1-r1 '([1] [-1]),
		offsets-n2-r1 '([0 1] [1 0] [0 -1] [-1 0]),
		offsets-n2-r2 '([0 1] [1 0] [0 -1] [-1 1] [1 1] [1 -1] [-1 -1] [-1 0]),
		offsets-n3-r1 '([0 0 1] [0 1 0] [1 0 0] [0 0 -1] [0 -1 0] [-1 0 0]),
		offsets-n3-r2 '([0 0 1] [0 1 0] [1 0 0] [0 0 -1] [0 -1 0] [-1 0 1]
						[0 1 1] [1 1 0] [1 0 1] [0 1 -1] [1 -1 0] [-1 0 -1]
						  [0 -1 1] [-1 1 0] [1 0 -1] [0 -1 -1] [-1 -1 0] [-1 0 0]),
		offsets-n3-r3 '([0 0 1] [0 1 0] [1 0 0] [0 0 -1] [0 -1 0] [-1 0 1] [0 1 1]
						  [1 1 0] [1 0 1] [0 1 -1] [1 -1 0] [-1 0 -1] [0 -1 1]
							[-1 1 0] [1 0 -1] [0 -1 -1] [-1 -1 1] [-1 1 1] [1 1 1]
							  [1 1 -1] [1 -1 1] [-1 1 -1] [1 -1 -1] [-1 -1 -1] [-1 -1 0]
								[-1 0 0])]
	(is (= (get-neighborhood-mask 1 1) offsets-n1-r1))
	(is (= (get-neighborhood-mask 2 1) offsets-n2-r1))
	(is (= (get-neighborhood-mask 2 2) offsets-n2-r2))
	(is (= (get-neighborhood-mask 3 1) offsets-n3-r1))
	(is (= (get-neighborhood-mask 3 2) offsets-n3-r2))
	(is (= (get-neighborhood-mask 3 3) offsets-n3-r3))))

(deftest translate-mask-test
  (let [neighborhood-x1-y1-r1 '([1 2] [2 1] [1 0] [0 1])]
	(is (= (translate-mask (get-neighborhood-mask 2 1) [1 1]) neighborhood-x1-y1-r1))))

(deftest interior-site?-test
  (is (true? (interior-site? [10 10] [5 5])))
  (is (true? (interior-site? [10 10] [0 0])))
  (is (false? (interior-site? [10 10] [5 10])))
  (is (true? (interior-site? [10] [5])))
  (is (true? (interior-site? [10 10 10] [5 5 5]))))

(deftest get-neighboring-sites-test
  (let [neighbors-16-16-2-1-0-0 '([0 1] [1 0]),
		neighbors-16-16-2-1-15-15 '([15 14] [14 15]),
		neighbors-16-16-2-1-7-7 '([7 8] [8 7] [7 6] [6 7])]
	(is (= (get-neighboring-sites [16 16] (get-neighborhood-mask 2 1) [0 0]) neighbors-16-16-2-1-0-0))
	(is (= (get-neighboring-sites [16 16] (get-neighborhood-mask 2 1) [15 15]) neighbors-16-16-2-1-15-15))
	(is (= (get-neighboring-sites [16 16] (get-neighborhood-mask 2 1) [7 7]) neighbors-16-16-2-1-7-7))))

(deftest get-neighboring-offsets-test
  (let [offsets-16-16-2-1-0 '(16 1),
		offsets-16-16-2-2-0 '(16 1 17)
		offsets-16-16-2-2-17 '(33 18 1 32 34 2 0 16)
		img2d (make-blobby-image-2d 16)
		img3d (make-blobby-image-3d 8)
		offsets-8-8-8-3-2-0 '(64 8 1 72 9 65)
		offsets-8-8-8-3-2-72 '(136 80 73 8 64 144 81 137 16 65 128 9 0)
		offsets-8-8-8-3-2-219 '(283 227 220 155 211 282 291 228 284 163 212 154 275 226 156 147 210 218)]
	(is (= (get-neighboring-offsets [16 16] [1 16] (get-neighborhood-mask 2 1) 0) offsets-16-16-2-1-0))
	(is (= (get-neighboring-offsets [16 16] [1 16] (get-neighborhood-mask 2 2) 0) offsets-16-16-2-2-0))
	(is (= (get-neighboring-offsets [16 16] [1 16] (get-neighborhood-mask 2 2) 17) offsets-16-16-2-2-17))
	(is (= (with-image-get-neighboring-offsets img2d (get-neighborhood-mask 2 1) 0) offsets-16-16-2-1-0))
	(is (= (with-image-get-neighboring-offsets img2d (get-neighborhood-mask 2 2) 0) offsets-16-16-2-2-0))
	(is (= (with-image-get-neighboring-offsets img2d (get-neighborhood-mask 2 2) 17) offsets-16-16-2-2-17))
	(is (= (with-image-get-neighboring-offsets img3d (get-neighborhood-mask 3 2) 0) offsets-8-8-8-3-2-0))
	(is (= (with-image-get-neighboring-offsets img3d (get-neighborhood-mask 3 2) 72) offsets-8-8-8-3-2-72))
	(is (= (with-image-get-neighboring-offsets img3d (get-neighborhood-mask 3 2) 219) offsets-8-8-8-3-2-219))
))

(deftest get-filtered-neighborhood-test
  (let [im4 (make-blobby-image-2d 4),
		mask_2_3 (get-neighborhood-mask 2 3),
		even-valued-neighbors-of-offset-6 (set '(1 2 3 5 7))]
	(= (set (get-filtered-neighborhood im4 mask_2_3 even? 6)) even-valued-neighbors-of-offset-6)))
