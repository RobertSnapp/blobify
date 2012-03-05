;;; project.clj is part of the source code for blobify.

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

(defproject blobify "0.1"
  :description "A prototype of the component tree datastructure and related tools for analyzing grayscale voxel arrays."
  :dependencies [[fiji-local/ij "1.0"]
                 [fiji-local/LSM_Toolbox "4.0g"]
                 [fiji-local/LSM_Reader "4.0g"]
                 [org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [joda-time "1.6"]
                 [clj-time "0.3.0"]
                 [fs "0.2.0"]
                 ]
  :dev-dependencies [[swank-clojure "1.4.0-SNAPSHOT"]
                     [clojure-source "1.2.0"]]
  :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
  :repositories {"clojure-releases"
                 "http://build.clojure.org/releases"}
  :aot [blobify.bounding-boxes
        blobify.core
		  blobify.image
		  blobify.main
		  blobify.polynomial
		  blobify.utils
        blobify.vectors]
  :main blobify.main)
