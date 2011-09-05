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
;;; java -jar blobify-0.1-standalone.jar <options>
;;;
;;; Command-line options:
;;;
;;; -s <int>        Set the random number seed to <int>

(ns blobify.main
  (:import (java.awt GridBagLayout GridBagConstraints))
  (:import (javax.swing JFrame JPanel JLabel ImageIcon))
  (:import (java.io BufferedReader FileReader))
  (:import (java.util Calendar Date ))
  (:use [blobify.image :only (make-blobby-image-2d
                              make-blobby-image-3d
                              open-stack-image
                              get-dimensions
                              get-dimensionality
                              get-max
                              get-pixel-site
                              get-pixel-volume
                              get-offset-of-site
                              get-scale-factors
                              get-site-of-offset
                              get-size
                              cumulative-histogram
                              inverse-cumulative-histogram)]
        [blobify.core :only (chop-intensity
                             display-ctree-3d
                             distance-table
                             export-rendered-ctree-3d
                             get-distances-from-pivot
                             make-ctree
                             make-summary
                             render-ctree-3d
                             prune
                             sift
                             trim
                             print-distance-table)]
        [blobify.utils :only (ave
                              dbg
                              debug
                              floor-rem
                              int-to-ubyte
                              ubyte-to-int
                              get-directory
                              get-filename
                              get-filename-root
                              pos-if
                              seq2redundant-map)]
        [blobify.vectors :only (v+)]
        [clojure.contrib.command-line :only (with-command-line)]
        [clojure.contrib.except :only (throw-if)]
        [clojure.contrib.pprint :only (cl-format)]
        [clojure.contrib.duck-streams :only (append-spit
                                             pwd
                                             read-lines
                                             with-out-writer)]
        [clj-time.core :only (now year month day hour minute sec)]
        [fs :only (listdir)]
        clojure.test)
  (:gen-class)
  )


;;; Test images

(def m568 "/Users/snapp/research/data/cellNuclei/lsmData/m568/m568_1aaa.lsm")
(def m568c "/Users/snapp/research/data/cellNuclei/lsmData/m568/fiji/save/m568_1aaa_crop.tif")
(def m567_2ll "/Users/snapp/research/data/cellNuclei/ctree/tif/m567_2ll.tif")
(def m588_1h "/Users/snapp/research/data/cellNuclei/ctree/tif/m588_1h.tif")
(def m567_2kk "/Users/snapp/research/data/cellNuclei/ctree/tif/m567_2kk.tif")
(def m567_2nn "/Users/snapp/research/data/cellNuclei/ctree/tif/m567_2nn.tif")
(def m675_2d "/Users/snapp/research/data/cellNuclei/ctree/tif/m675_2d.tif")
(def m675_2g "/Users/snapp/research/data/cellNuclei/ctree/tif/m675_2g.tif")
(def m675_2k "/Users/snapp/research/data/cellNuclei/ctree/tif/m675_2k.tif")
(def m675_4a "/Users/snapp/research/data/cellNuclei/ctree/tif/m675_4a.tif")

;;; need new window limits
(def m583_1e ["/Users/snapp/research/data/cellNuclei/lsmData_2006/m583_1e.lsm" [384 320 0 256 320 30]])
(def m583_2e ["/Users/snapp/research/data/cellNuclei/lsmData_2006/m583_2e.lsm" [384 396 0 212 352 30]])
(def m585_1h_G3 ["/Users/snapp/research/data/cellNuclei/lsmData_2006/m585_1h_G3.lsm" [256 180 0 512 512 30]])
(def m691_1g ["/Users/snapp/research/data/cellNuclei/lsmData_2006/m691_1g.lsm" [410 314 0 291 398 30]])
(def m714_4a ["/Users/snapp/research/data/cellNuclei/lsmData_2006/m714_4a.lsm" [336 462 0 378 341 30]])
(def m714_4b ["/Users/snapp/research/data/cellNuclei/lsmData_2006/m714_4b.lsm" [256 256 0 512 512 30]])
(def m714_4g ["/Users/snapp/research/data/cellNuclei/lsmData_2006/m714_4g.lsm" [384 384 0 512 512 30]])


;;; File rosters
(def lsmdir2006 "/Users/snapp/research/data/cellNuclei/lsmData_2006/fileRoster3d.in")
(def short2006 "/Users/snapp/research/data/cellNuclei/lsmData_2006/shortRoster3d.in")
(def lsmdir2007 "/Users/snapp/research/data/cellNuclei/lsmData/fileRoster3d.in")


(defn next-file-version-index
  "Returns the next unused file-version-index (i.e., numerical suffix) used for files that share the
common extension ext (arg1) in the subdirectory indicated by path (arg2)."
  [path]
  (let [[dir filename] ((juxt get-directory get-filename) path)]
    (inc (apply max 0
                (map (partial (comp read-string first re-seq) #"[0-9]+$")
                     (filter (partial re-find (re-pattern (str filename "[0-9]+$")))
                             (listdir dir)))))))

(defn bb-to-roi
  "Computes the region of interest (a vector of two offsets) of the given bounding box bb."
  [image bb]
  (let [d (get-dimensionality image)
        lower-site (take d bb)
        deltas (drop d bb)
        upper-site (map min (map dec (get-dimensions image))
                        (v+ lower-site (map dec deltas)))]
    (vec (map (partial get-offset-of-site image) (list lower-site upper-site)))))

(defn process-image-file
  "Applies a sequence of operations to the indicated image or image stack:
   1. The image file is opened.
   2. Using min-intensity as the threshold value, a compoment tree is constructed.
   3. Components that are smaller than 50 voxels are dropped. 
   4. The pairwise distances between component centers are computed.
   5. A list is printed that contains the following data:
      The number of compoents
      The average component size
      The minimum distance
      The maximum distance
      The average distance.

 The arguments in order:
   path:          a complete path to a tif or lsm file that contains an image stack;
   bbox:          a six-dimensional vector of ints that describes the region of interest,
                  of the form [x y z delta-x delta-y delta-z];
   min-intensity: minimium voxel intensity that will be used for computing the image histogram within
                  the function make-ctree.
   sift-volume    measured in cubic microns, the size of the smallest component that will be retained
                  in the component tree.
   verbose:       if true, then progress messages are sent to *out*."
  [path bbox min-intensity sift-volume verbose]
  (let [ _ (if verbose (printf "Opening image %s..." path))
        stack (open-stack-image path 0 2) ; DAPI channel
        dimensions (get-dimensions stack)
        ;; _ (if verbose (printf "done.\nComputing cumulative histogram..."))
        d (get-dimensionality stack)
        roi (bb-to-roi stack bbox)
        ;; _ (println "roi = " roi)
        ;; c-hist (cumulative-histogram stack roi)
        ;; _ (if verbose (printf "done.\n"))
        sift-size  (int (/ sift-volume (get-pixel-volume stack)))
        min-child-size (* 4 sift-size)
        [bin-threshold ct] (make-ctree stack 1 roi min-intensity verbose)
        ctrees (sift (apply concat (map (partial prune 0.25 min-child-size)
                                  (sift ct sift-size))) sift-size)
        png-filename (str (get-filename-root path) ".png")
        _ (if verbose (printf "Creating projected image in file %s..." png-filename))
        _ (export-rendered-ctree-3d png-filename stack ctrees roi)
        _ (if verbose (printf "done.\n Making summary and distance table:\n"))
        summary (sort-by :size >  (map (partial make-summary stack) ctrees))
        _ (if verbose (print-distance-table (map :mean summary)))
        n-components (count ctrees)
        [min-size max-size ave-size] (if (pos? n-components)
                                       (apply (juxt min max (comp float ave)) (map :size summary))
                                       [0 0 0])
        d-table (distance-table (map :mean summary))
        [min-distance max-distance ave-distance] (if (< 1 n-components)
                                                   (apply (juxt min max ave) (apply concat d-table))
                                                   [0 0 0])
        ave-min-distance (if (< 1 n-components)
                           (/ (apply +
                                     (map (partial
                                           (comp (partial apply min) get-distances-from-pivot)
                                           d-table) (range n-components))) n-components)
                           -1.0)
          ]
    (list (get-scale-factors stack) 
          bin-threshold
          n-components min-size max-size ave-size min-distance max-distance ave-distance ave-min-distance)
    ))

(defn process-roster-file
  [roster-path image-path-prefix min-intensity sift-volume verbose]
  (let [[YY MM DD hh mm ss] ((juxt year month day hour minute sec) (now))
        time_stamp (str MM "/" DD "/" DD  " at " hh ":" mm ":" ss)
        output-file-index (next-file-version-index "out/ctree.out")
        output-file (str "out/ctree.out" output-file-index)]
    (append-spit output-file (cl-format false "blobify output generated at time ~A~%" time_stamp))
    (append-spit output-file (cl-format false "using roster file ~A with image-path-prefix ~A ~%"
                                        roster-path image-path-prefix))
    (append-spit output-file
                 (cl-format false "Fields with a * are given in microns. Other quantities are dimensionless.~2%"))
    
    (append-spit output-file
                 (cl-format false
                            "~32A ~10@A ~10@A ~10@A ~6{~6@A ~}~6@A ~8@A ~8@A ~8@A ~10@A ~10@A ~10@A ~10@A ~10@A~%"
                            "filename" "Voxel-Dx*" "Voxel-Dy*" "Voxel-Dz*" ["row" "col" "lay" "width" "height" "girth"]
                            "thresh" "nComps" "minSize" "maxSize" "aveSize"
                            "minDst*" "maxDst*" "aveDst*" "aveMinDst*"))
    (doseq [line (read-lines roster-path)]
      (let [tokens (re-seq #"[^\s,;]+" line)]
        (cond (= (first (first tokens)) \#)  (if verbose (println "Comment detected.")),
              (zero? (count tokens)) (if verbose (println "Blank line: ignored.")),
              (> (count tokens) 6)
              (let [image-file (nth tokens 0),
                    bbox6 (take 6 (map #(Integer/parseInt %) (drop 1 tokens))),
                    image-path (str image-path-prefix image-file),
                    _ (if verbose
                        (println "Processing file" image-path "with threshold" min-intensity
                                 "and bbox" bbox6))
                    results10 (process-image-file image-path bbox6 min-intensity sift-volume verbose)
                    _ (append-spit
                       output-file
                       (cl-format false
                                  "~32A ~3{~10,4F ~}~6{~6D ~}~6D ~{~8D ~8D ~8D ~10,2F ~10,4F ~10,4F ~10,4F ~10,4F~}~%"
                                  image-file (first results10) bbox6 (second results10) (drop 2 results10)))
                    ]),
              :else
              (println "Bad line read = " line))))))


(defn- splash
  "Displays the copyright and license information."
  []
  (println "blobify *** Copyright (C) 2011 Robert R. Snapp <snapp@cs.uvm.edu>")
  (println "This program comes with ABSOLUTELY NO WARRANTY. This is free")
  (println "software, and you are welcome to redistribute it under certain")
  (println "conditions."))

(defn- usage
  []
  (println "Usage: java -jar blobify-0.1-standalone.jar [options] <filename>"))

(defn -main [& args]
  (splash)

  ;; Parse any command line arguments
  (let [sift-volume 0.033179]
    (with-command-line args
    "Usage: java -jar blobify-0.1-standalone.jar [options] <filename>"
    [[verbose? V? "If set, send detailed messages to console."]
     [roster? R? "If set, the filename is a roster of files."]
     [path "The directory that contains the image files." ""]
     [min-intensity "The smallest intensity added to the component tree" "15"]
     [col "column offset" "0"]
     [row "row offset" "0"]
     [lay "layer offset" "0"]
     [width "number of columns" "0"]
     [height "number of rows" "0"]
     [girth "number of layers" "0"]
     trailing-args]
    (if (empty? trailing-args)
      (usage)
      (loop [the-args trailing-args]
        (if (empty? the-args)
          (System/exit 0)
          (if roster?
            (println (process-roster-file (first the-args) path
                                          (Integer/parseInt min-intensity)
                                          sift-volume verbose?))
            (println (first the-args)
                     col row lay width height girth min-intensity
                     (process-image-file (first the-args)
                                (map #(Integer/parseInt %) (vector col row lay width height girth))
                                (Integer/parseInt min-intensity)
                                sift-volume
                                verbose?))))
        (recur (rest the-args)))))))




