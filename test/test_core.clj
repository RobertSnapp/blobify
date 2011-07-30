;;; test_core.clj is part of blobify.
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
;;; Tests the core functions of blobify defined in blobify.core.

(ns test_core
  (:use blobify.core)
  (:use blobify.image)
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
