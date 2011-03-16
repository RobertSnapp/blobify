;;; test_core.clj
;;;
;;; Tests the core functions of clj-ctree defined in file
;;; clj_ctree/core.clj

(ns test_core
  (:use clj-ctree.core)
  (:use clj-ctree.image)
  (:use clojure.test))

#_(deftest ctree-test
  (let [im2d4 (make-blobby-image-2d 4),
		im3d4 (make-blobby-image-3d 4)
		im2d4-ctree-r1 (make-ctree im2d4 1 0),
		im3d4-ctree-r3 (make-ctree im3d4 3 0),
		im2d4-keys '(252 85 50 32 20 13 6 1 0)
		im3d4-keys '(165 104 61 22 17 13 12 6 4 2 1 0)]
	(is (= (keys im2d4-ctree-r1) im2d4-keys))
	(is (= (keys im3d4-ctree-r3) im3d4-keys))
	(is (= (apply count (get im2d4-ctree-r1 0)) 16))
	(is (= (apply count (get im3d4-ctree-r3 0)) 64))))
