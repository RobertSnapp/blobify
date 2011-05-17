;;; main.clj
;;; Created by Robert R. Snapp
;;;
;;; A driver for the component tree analysis library
;;;
;;; Syntax:
;;; java -jar componentTree-standalone.jar <options>
;;;
;;; Command-line options:
;;;
;;; -s <int>        Set the random number seed to <int>

;;; Copyright (c) 2010

(ns clj-ctree.main
  (:import (java.awt GridBagLayout GridBagConstraints))
  (:import (javax.swing JFrame JPanel JLabel ImageIcon))
  (:use [clj-ctree.image :only (make-blobby-image-2d
                                make-blobby-image-3d
                                open-stack-image
                                get-max
                                get-pixel-site
                                get-size
                                cumulative-histogram
                                inverse-cumulative-histogram)]
        [clj-ctree.core :only (make-ctree render-ctree-3d make-summary print-distance-table)]
        [clj-ctree.utils :only (dbg debug floor-rem int-to-ubyte ubyte-to-int
                                    get-directory get-filename pos-if seq2redundant-map)]
        [clojure.contrib.command-line :only (with-command-line)]
        [clojure.contrib.except :only (throw-if)]
        [clojure.contrib.pprint :only (cl-format)]
        clojure.test)

  ;;(:use clojure.contrib.command-line)
  ;; (:use clojure.contrib.duck-streams)
  ;; (:use [clojure.contrib.except :only (throw-if)])
  ;; (:use clojure.contrib.pprint)
  
                                        ;(:gen-class)
  )

(def EMPTY_TILE (ImageIcon. "empty.png"))

;;; Test images

(def m568 "/Users/snapp/data/cellNuclei/lsmData/m568/m568_1aaa.lsm")
(def m568c "/Users/snapp/data/cellNuclei/lsmData/m568/fiji/save/m568_1aaa_crop.tif")
(def m567 "/Users/snapp/data/cellNuclei/ctree/tif/m567.tif")
(def m588 "/Users/snapp/data/cellNuclei/ctree/tif/m588.tif")
;;; Evaluate (run-tests ') 
(deftest main-test
  (def img16 (make-blobby-image-2d 16))
  (def s1 (open-stack-image m567 0 2))
  (is (= (count s1) 5))
  (def s2 (open-stack-image m588 0 2))
  (is (= (count s2) 5))
  (def ch2 (cumulative-histogram s2))
  )

(deftest s1-test
  (def s1 (open-stack-image m567 0 2))
  (is (= (count s1) 5))
  (def ch1 (cumulative-histogram s1))
  (inverse-cumulative-histogram ch1 0.98))
(defn empty-tile []
  (JLabel. EMPTY_TILE))

(defn simple-grid
  "Creates a grid of WIDTH + 1 columns and HEIGHT + 1 rows where each cell contains the result of a call to (empty-tile) and adds it to the panel."
  [panel width height]
  (let [constraints (GridBagConstraints.)]
	(loop [x 0 y 0]
	  (set! (. constraints gridx) x)
	  (set! (. constraints gridy) y)
	  (. panel add (empty-tile) constraints)
	  (cond (and (= x width) (= y height)) panel
			(= y height) (recur (+ x 1) 0)
			true (recur x (+ y 1))))))

(defn -main [& args]
  (println "Running clojure version "	 (clojure-version))
  (println "Welcome to Component Tree Analysis!")

   ;; Parse any command line arguments
  (with-command-line args
	"Usage: ctree [options]"
	[[seed "random-seed" "current system time in ms."]
	 trailing-args]

	;; Print any additional command line arguments to the repl as being ignored.
	(loop [the-args trailing-args]
	  (when-not (empty? the-args)
		(println "WARNNG: Ignoring " (first the-args))
		(recur (rest the-args))))

	(let [seed-value (if (= seed "current system time in ms.")
					   (System/currentTimeMillis) (int seed))
		  image (make-blobby-image-2d 128)]
	  (println "seed = " seed-value)
	  (doto (JFrame. "Image")
		(.setSize 200 200)
		(.setVisible true))
	  )))




