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

(ns snapp.componentTree.main
  (:import (java.awt GridBagLayout GridBagConstraints))
  (:import (javax.swing JFrame JPanel JLabel ImageIcon))
  (:use snapp.componentTree.image)
  (:use clojure.contrib.command-line)
; (:use clojure.contrib.duck-streams)
  (:use [clojure.contrib.except :only (throw-if)])
  (:use clojure.contrib.pprint)
  (:gen-class))

(def EMPTY_TILE (ImageIcon. "empty.png"))

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
		  image (make-image-from-fn [64 64] intensity-ramp)]
	  (println "seed = " seed-value)
	  (doto (JFrame. "Image")
		(.setSize 200 200)
		(.setVisible true))
	  )))




