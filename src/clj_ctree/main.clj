;;; main.clj is part of the source code for blobify.
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
;;; A driver for the component tree analysis library
;;;
;;; Syntax:
;;; java -jar blobify-standalone.jar <options>
;;;
;;; Command-line options:
;;;
;;; -s <int>        Set the random number seed to <int>

(ns blobify.main
  (:import (java.awt GridBagLayout GridBagConstraints))
  (:import (javax.swing JFrame JPanel JLabel ImageIcon))
  (:import (java.io BufferedReader FileReader))
  (:import (java.util Calendar Date ))
  #_(:import (org.joda.time DateTime DateTimeZone Period Interval))
  (:use [blobify.image :only (make-blobby-image-2d
                                make-blobby-image-3d
                                open-stack-image
                                get-dimensions
                                get-max
                                get-pixel-site
                                get-offset-of-site
                                get-size
                                cumulative-histogram
                                inverse-cumulative-histogram)]
        [blobify.core :only (chop-intensity
                               make-ctree
                               render-ctree-3d
                               make-summary
                               print-distance-table)]
        [blobify.utils :only (dbg debug floor-rem int-to-ubyte ubyte-to-int
                                    get-directory get-filename pos-if seq2redundant-map)]
        [clojure.contrib.command-line :only (with-command-line)]
        [clojure.contrib.except :only (throw-if)]
        [clojure.contrib.pprint :only (cl-format)]
        [clojure.contrib.duck-streams :only (append-spit
                                             pwd
                                             read-lines
                                             with-out-writer)]
       #_[clojure.contrib.string :only (partition substring?)]
        [clj-time.core :only (now year month day hour minute sec)]
        [fs :only (listdir)]
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
(def m567_2ll "/Users/snapp/data/cellNuclei/ctree/tif/m567_2ll.tif")
(def m588_1h "/Users/snapp/data/cellNuclei/ctree/tif/m588_1h.tif")
(def m567_2kk "/Users/snapp/data/cellNuclei/ctree/tif/m567_2kk.tif")
(def m567_2nn "/Users/snapp/data/cellNuclei/ctree/tif/m567_2nn.tif")
(def m675_2d "/Users/snapp/data/cellNuclei/ctree/tif/m675_2d.tif")
(def m675_2g "/Users/snapp/data/cellNuclei/ctree/tif/m675_2g.tif")
(def m675_2k "/Users/snapp/data/cellNuclei/ctree/tif/m675_2k.tif")
(def m675_4a "/Users/snapp/data/cellNuclei/ctree/tif/m675_4a.tif")

(def lsmdir2006 "/Users/snapp/data/cellNuclei/lsmData_2006/fileRoster3d.in")
(def short2006 "/Users/snapp/data/cellNuclei/lsmData_2006/shortRoster3d.in")
(def lsmdir2007 "/Users/snapp/data/cellNuclei/lsmData/fileRoster3d.in")

(defn analyze
  [path fraction]
  (let [_ (printf "Opening image %s..." path)
        stack (open-stack-image path 0 2)  ; access only the DAPI channel.
        _ (printf "done.\nComputing cumulative histogram...")
        c-hist (cumulative-histogram stack)
        _ (printf "done.\nComputing adaptive threshold...")
        threshold (inverse-cumulative-histogram c-hist fraction)
        _ (printf "threshold = %d, done.\nComputing ctree:\n" threshold)
        ctrees (make-ctree stack 3 threshold)
        _ (printf "Creating projected image...")
        _ (render-ctree-3d stack ctrees)
        _ (printf "done.\n Making summary and distance table:\n")
        summary (sort-by :size >  (map (partial make-summary stack) (filter #(< 10 (:size %)) ctrees)))
        _ (print-distance-table (map :mean summary))
        _ (printf "done.\n")]
    summary))



(defn next-file-version-index
  "Returns the next unused file-version-index (i.e., numerical suffix) used for files that share the
common extension ext (arg1) in the subdirectory indicated by path (arg2)."
  [path]
  (let [[dir filename] ((juxt get-directory get-filename) path)]
    (inc (apply max 0
                (map (partial (comp read-string first re-seq) #"[0-9]+$")
                     (filter (partial re-find (re-pattern (str filename "[0-9]+$")))
                             (listdir dir)))))))


(defn process-file
  [path fraction]
  (let [[YY MM DD hh mm ss] ((juxt year month day hour minute sec) (now))
        time_stamp (str MM "/" DD "/" DD  " at " hh ":" mm ":" ss)
        output-file-index (next-file-version-index "out/ctree.out")
        output-file (str "out/ctree.out" output-file-index)
        source-directory (get-directory path)]
    (append-spit output-file (cl-format false "~A~%" time_stamp))
    (doseq [line (read-lines path)]
      (let [tokens (re-seq #"[^\s,;]+" line)]
        (cond (= (first (first tokens)) \#)
              (dorun (append-spit output-file (cl-format false "~s~%" line))
                     (println "Comment detected.")),
              (zero? (count tokens)) (println "Blank line: ignored."),
              (> (count tokens) 6)
              (let [image-file (nth tokens 0)
                    [column row layer width height thickness]
                    (take 6 (map #(Integer/parseInt %) (drop 1 tokens)))
                    image-path (str source-directory image-file)]
                (println "Processing file " image-path "...")
                (let [_ (printf "Opening image %s..." image-path)
                      stack (open-stack-image image-path 0 2) ; DAPI channel
                      _ (printf "done.\nComputing cumulative histogram...")
                      real-thickness (nth (get-dimensions stack) 2)
                      roi-sites (vector (vector column row 0)
                                        (vector (+ column (dec width))
                                                (+ row (dec height))
                                                (dec real-thickness)))
                      roi (map (partial get-offset-of-site stack) roi-sites)
                      c-hist (cumulative-histogram stack roi)
                      _ (printf "done.\nComputing adaptive threshold...")
                      threshold (inverse-cumulative-histogram c-hist fraction)
                      _ (printf "threshold = %d, done.\nComputing ctree:\n" threshold)
                      ctrees (make-ctree stack 3 threshold roi)
                      _ (printf "Creating projected image...")
                      _ (render-ctree-3d stack ctrees)
                      _ (printf "done.\n Making summary and distance table:\n")
                      summary (sort-by :size >  (map (partial make-summary stack) (filter #(< 10 (:size %)) ctrees)))
                      _ (print-distance-table (map :mean summary))
                      _ (printf "Appending output file.")
                      _ (append-spit output-file (format "%s "))
                      _ (printf "done.\n")])),
              :else
              (println "Bad line read = " line))))))


(deftest main-test
  (def img16 (make-blobby-image-2d 16))
  (def s1 (open-stack-image m567_2ll 0 2))
  (is (= (count s1) 5))
  (def s2 (open-stack-image m588_1h 0 2))
  (is (= (count s2) 5))
  (def ch2 (cumulative-histogram s2))
  )

(deftest s1-test
  (def s1 (open-stack-image m567_2ll 0 2))
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




