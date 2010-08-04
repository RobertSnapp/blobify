(ns test_image
  (:use snapp.componentTree.image)
  (:use clojure.test))


;;; A few functions for making test images

(defn checker-board
  "A function of a variable number of arguements that produces a checkerboard pattern consisting
   of cells that have sides of 4. Cells with odd parity index are set to 0, otherwise they are
   set to an amplitude between 0 and 255 according to the absolute value of the cosine of
   the product of the indicies."
  [& args]
  (let [ints (map #(int (/ % 4)) args)]
	(if (even? (apply + ints))
	  (* 255 (Math/abs (Math/cos (apply * ints))))
	  0)))



(deftest image2d_test
  (let [im (make-image2d-from-fn 16 32 checker-board)]
	(is (= (:rows im) 16))
	(is (= (:cols im) 32))
	(is (= (count (:raster im)) (* (:rows im) (:cols im))))
	(is (= (get-image2d-pixel im 0 0) 255))))


(deftest image3d_test
  (let [im (make-image3d-from-fn 8 16 32 checker-board)]
	(is (= (:layers im) 8))
	(is (= (:rows im) 16))
	(is (= (:cols im) 32))
	(is (= (count (:raster im)) (* (:layers im) (:rows im) (:cols im))))
	(is (= (get-image3d-pixel im 0 0 0) 255))))
