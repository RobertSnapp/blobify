;;; core.clj is part of blobify.
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


;;; The implementation of the component tree for gray-level images.

(ns blobify.core
  (:require (clojure.java.io))
  (:import (java.awt.image BufferedImage)
           (java.awt Color Graphics Dimension)
           (java.io File)
           (javax.imageio IIOImage ImageIO)
           (javax.imageio.plugins.jpeg JPEGImageWriteParam)
           (javax.swing JPanel JFrame JLabel)
           (ij ImagePlus)
           (org.imagearchive.lsm.reader Reader))
  (:use [blobify.image :only (get-dimensionality
                              get-filtered-neighborhood
                              get-neighborhood-mask
                              get-pixel
                              get-pixel-site
                              get-offset-of-site
                              get-offsets
                              get-scale-factors
                              get-site-of-offset
                              get-size
                              scale-site
                              with-image-get-neighboring-offsets)]
        [blobify.utils :only (dbg
                              dbg-indent
                              debug
                              least-above
                              seq2redundant-map
                              square
                              tree-search
                              when-dbg)]
        [blobify.vectors :only (flat-vector-auto-product
                                l2-distance
                                interset-distance
                                square-distance
                                v+
                                v-
                                vector-interp
                                vector-square)]
        [blobify.graphics :only (get-indexed-rgb)]
        [clojure.set :only (intersection
                            union)]
        [clojure.contrib.pprint :only (cl-format)]
        clojure.test
		 ))

(defn bin-by-intensity
  "Given an image (arg1) bin-by-intensity returns an intensity map of the image, in which
each intensity value is represented by a key (sorted in decreasing order), followed by a list
of offsets corresponding to each intensity value."
  ([image]
     (letfn [(key-value-pair [x] (vector (get-pixel image x) x))]
       (seq2redundant-map (get-offsets image) key-value-pair conj :sort-down)))
  ([image roi]
     (letfn [(key-value-pair [x] (vector (get-pixel image x) x))]
       (seq2redundant-map (get-offsets image roi) key-value-pair conj :sort-down))))


;; (debug :make-ctree-gbn) ; remove/insert leading comment to toggle debugging
;; (debug :make-ctree-cc) ; remove/insert leading comment to toggle debugging

;;; In the following, we try a more concise implementation in which each node in the component tree
;;; is implemented as a struct. The intensity should be an integer corresponding to the monochrome
;;; pixel intensity of the pixels that are included with this node. The offsets is a set 
;;; of these pixels in the current raster vector. 

;; box represents a boudning box and should be a vector of the form [lower-offset upper-offset]

(defrecord ComponentTreeNode [intensity offsets size mean variance energy box children])

;;; Note that many of the functions below require the image as an input
;;; parameter. The dimension-products field of each image record is used to
;;; convert raster offsets into site lattice coordinates, and vice versa.

(defn offset-inside-box?
  "Returns true if and only if the lattice site referenced by offset in the current image lies
within the closed bounding box represented by the lower and upper offsets."
  [image offset [lower upper]]
  (let [[lower-site upper-site offset-site] (map #(get-site-of-offset image %) (list lower upper offset))]
    (every? true? (map #(<= %1 %2 %3) lower-site offset-site upper-site))))

(defn site-inside-box?
  "Returns true if and only if for the indicated image (arg1) the indicated lattice site (arg2)
lies within the closed bounding box defined by the lower and upper offset vector (arg3)."
  [image site [lower upper]]
  (let [[lower-site upper-site] (map #(get-site-of-offset image %) (list lower upper))]
    (every? true? (map #(<= %1 %2 %3) lower-site site upper-site))))


;;; Obsoleted by ctree-contains-offset-at-site?
#_(defn ctree-contains-offset?
  "Returns true if the ctree contains the indicated intensity-offset pair"
  [ctree intensity offset]
  (cond (empty? ctree) false,
        (< intensity (:intensity ctree)) false,
        (= intensity (:intensity ctree)) (contains? (:offsets ctree) offset),
        :else (some #(ctree-contains-offset? % intensity offset) (:children ctree))))

(defn ctree-contains-offset-at-site?
  "Returns true if the ctree contains the indicated intensity-offset pair"
  [image ctree intensity offset]
  (let [site (get-site-of-offset image offset)]
    (loop [ctrees (list ctree)]
      (if (empty? ctrees)
        false
        (let [next-tree (first ctrees)]
          (cond  (and (= intensity (:intensity next-tree))
                      (contains? (:offsets next-tree) offset)) true,
                 (< intensity (:intensity next-tree)) (recur (rest ctrees)),
                 (and (:box ctree) (not (site-inside-box? image site (:box ctree))))
                 (recur (rest ctrees)),
                 :else (recur (concat (remove nil? (:children next-tree)) (rest ctrees)))))))))

(defn merge-boxes
  "Merge the two given bounding boxes: A new bounding box is returned that contains b1 and b2."
  [image b1 b2]
  (let [[l1 u1]  (map #(get-site-of-offset image %) b1)
        [l2 u2]  (map #(get-site-of-offset image %) b2)
        min-site (vec (map #(min %1 %2) l1 l2))
        max-site (vec (map #(max %1 %2) u1 u2))]
    (vec (map #(get-offset-of-site image %) (list min-site max-site)))))

(defn merge-ctrees
  "Creates a new ctree created by merging the two indicated ctrees, indicated that the components
represented by each unite at the minimum intensity of the two. Note that the function is symmetric."
  ;; [ct1 ct2]
  [image
   {i1 :intensity set1 :offsets n1 :size m1 :mean v1 :variance e1 :energy b1 :box ch1 :children :as ct1}
   {i2 :intensity set2 :offsets n2 :size m2 :mean v2 :variance e2 :energy b2 :box ch2 :children :as ct2}]
  {:pre [(map #(or (nil? %) (apply <= %)) (list b1 b2))]
   :post [(apply <= (:box %))]},
  (dbg :merge-ctrees "~&ct1-> ~a~% ct2-> ~a~%~%" (:size ct1) (:size ct2))
  (letfn [(redefine-box-if-nil
            [box offsets] (if box box (let [x (first offsets)] (vector x x))))]
    (let [n12 (+ n1 n2)
          r12 (float (/ n1 n12)) ; interpolation parameter
          m12 (vector-interp m2 m1 r12) ; r12 = 0 implies m12 = m2
         ;; v12 (/ (+ (* n1 (+ v1 (vector-square (v- m1 m12))))
         ;;           (* n2 (+ v2 (vector-square (v- m2 m12))))) n12)
          v12 (vector-interp v2 v1 r12) ; the weighted second moment
          e12 (+ e1 e2)
          b1x (redefine-box-if-nil b1 set1)
          b2x (redefine-box-if-nil b2 set2)
          b12 (merge-boxes image b1x b2x)]
      (cond (< i1 i2) (ComponentTreeNode. i1 set1 n12 m12 v12 e12 b12 (conj ch1 ct2)),
            (= i1 i2) (ComponentTreeNode. i1 (union set1 set2)  n12 m12 v12 e12 b12 (concat ch1 ch2)),
            :else (ComponentTreeNode. i2 set2 n12 m12 v12 e12 b12 (conj ch2 ct1))))))

(defn pprint-ctree
  ([stm level {:keys [intensity offsets children] :as ctree}]
	 (let [indentation (apply str (take (* 3 level) (repeat \space)))]
	   (if (empty? ctree)
		 (cl-format stm "~&~a []" indentation),
		 (dorun (cl-format stm "~&~a[~a, ~a:~%" indentation intensity offsets)
				(dorun (map #(pprint-ctree stm (inc level) %) children))))))
  ([stm ctree] (pprint-ctree stm 0 ctree))
  ([ctree] (pprint-ctree true 0 ctree)))

;; In the following, a component is implemented as a ctree. Components that include 
;; pixels with intensities greater than or equal to that of the indicated offset (arg2)
;; will be collected and returned as a list of ctree-nodes. The function begins with a list
;; of components, called ctrees (arg1), that are defined for the minimum pixel
;; intensity greater than that of offset (arg2). (For the initial iteration, ctrees (arg1)
;; is set to nil.) The main body of make-ctree uses reduce to merge components that
;; become topologically adjacent as the threshold is decreased. collect-compoonents
;; scans the list of active ctrees, merging those that contain a topological neighbor of
;; the current offset, and returns the result, as a list of ctrees.

(defn make-ctree
  "Given an Image (arg1), an integer hamming radius (arg2), and a minimum intensity
threhsold, make-ctree generates a component tree for the image assuming the topology specified
by the second argument (adjacent lattice sites within the hamming radius are topologically
connected). Pixel intensities below min-intensity are ignored."
  ([image max-hamming-radius roi min-intensity verbose]
     (if verbose (printf "Sorting %d %d-dimensional pixels into intensity bins ... "
                         (get-size image) (get-dimensionality image)))
     (let [bins0 (filter #(> (first %) min-intensity) (bin-by-intensity image roi))
           histogram  (for [[key val] bins0] [key (count val)])
           energy (apply + (map (fn [[k v]] (* k v)) histogram))
           size (apply + (map (fn [[k v]] v) histogram))
           mean (float (/ energy size))
           _ (if verbose (println "Average intensity = " mean))
           bin-threshold (loop [value 255 sum 0 h histogram]
                           (if (or (empty? h) (> sum (* size 0.05)))
                             value
                             (recur (first (first h)) (+ sum (second (first h))) (rest h))))
           _ (if verbose (println "bin-threshold = " bin-threshold))
           bins (filter #(>= (first %) bin-threshold) bins0)
           mask (get-neighborhood-mask (get-dimensionality image) max-hamming-radius)
           k-values (keys bins)]
       (if verbose (print "... done.\n\n"))
       (letfn [(get-bright-neighbors	; return a list of offsets with intensities that are greater
                 [offset]				   ; than or equal to that of the indicated offset (arg1).
                (let [threshold (get-pixel image offset)
                      neighbors (map #(vector (get-pixel image %) %)
                                     (with-image-get-neighboring-offsets image mask offset))
                      brights (filter #(<= threshold (first %)) neighbors)]
                  (dbg :make-ctree-gbn "(get-bright-neighbors ~a) => ~s~%" offset brights)
                  brights))
               (collect-components    ; merge ctrees that contain a neighbor of offset
                [ctrees offset]
                (let [neighbors (get-bright-neighbors offset)
                      intensity (get-pixel image offset)
                      site (get-site-of-offset image offset)
                      scatter (flat-vector-auto-product site)]
                  (dbg :make-ctree-cc "active-sets-> ~a, offset-> ~a:~%" ctrees offset)
                  ;; In the loop, current-set is the component that contains offset. The other-sets
                  ;; is a list of components (taken from open-sets) that are
                  ;; not adjacent to offset. 
                  (loop [open-roots ctrees,
                         local-root (ComponentTreeNode. intensity
                                                        (set (list offset))
                                                        1 
                                                        site  ; the mean
                                                        scatter ; the 2nd moment of the site vector
                                                        intensity
                                                        nil   ; initial bounding box.
                                                        nil   ; initial children
                                                        ),
                         other-roots nil]
                    (if (empty? open-roots)
                      (let [consolidated-ctrees (conj other-roots local-root)]  ; let enables dbg side effect.
                        (dbg-indent :make-ctree-cc 1 "returning ~a ~%" consolidated-ctrees)
                        consolidated-ctrees)
                      (let [s (first open-roots)]
                        (if (some #(ctree-contains-offset-at-site? image s (first %) (second %)) neighbors )
                          (do (dbg :make-ctree-cc "offset ~a adjoins  ~s. (neighbors= ~a)~%" offset s neighbors)
                              (recur (rest open-roots) (merge-ctrees image local-root s) other-roots))
                          (do (dbg :make-ctree-cc "offset ~a disjoins ~s. (neighbors= ~a)~%" offset s neighbors)
                              (recur (rest open-roots) local-root (conj other-roots s)))))))))]
         (if verbose (printf "  Intensity \t    Count \t  Components:\n"))
         (loop [ct nil b bins]
           (when-dbg :make-ctree (dorun (map #(pprint-ctree %) ct)))
           (if (empty? b)
             [bin-threshold ct]
             (let [[k offsets] (first b),
                   k-sets (reduce #(collect-components %1 %2) ct offsets)]
               (if verbose (printf "%10d \t %10d \t %10d\n" k (count offsets) (count k-sets)))
               (recur k-sets (rest b)))))))))

(defn chop-intensity
  [ctrees threshold]
  (letfn [(filter-fn [ctree] (<= threshold (:intensity ctree)))
		  (children-fn [ctree] (:children ctree))]
	(tree-search filter-fn children-fn ctrees)))

(defn trim
  "Select components that are less than (or equal to) the indicated size."
  [ctrees max-size]
  (letfn [(filter-fn [ctree] (>= max-size (:size ctree)))
		  (children-fn [ctree] (:children ctree))]
    (tree-search filter-fn children-fn ctrees)))

(defn sift
  "Selects components that are greater than or equal to the indicated size."
  [ctrees min-size]
  (filter #(<= min-size (:size %)) ctrees))

(defn prune
  "Given a component, prune follows the main trunk of its tree, returning a list of descendents that correspond
to a 20% or more split (if such a split occurs), or a list containing the original component (if not)."
  [split-ratio-threshold min-child-size ctree]
  (letfn [(goal-fn  [node]
            (let [children (:children node)]
              (and (> (count children) 1)
                   (let [split-ratio (float (/ (:size (second (sort-by :size > children)))
                                                (:size node)))]
                     (> split-ratio split-ratio-threshold)))))
          (successor-fn [node]
            (let [children (filter #(<= min-child-size (:size %)) (:children node))]
              (if (pos? (count children))
                (list (first (sort-by :size > children)))
                ())))]
    (let [result (tree-search ctree goal-fn successor-fn concat)]
    (if result
      (:children result)
      (list ctree)))))

;;; Functions that anayze a list of ctrees.

(defrecord NodeSummary [size mean variance energy])

(defn make-summary
  [image ctree]
  (NodeSummary. (:size ctree)
                (scale-site image (:mean ctree))
                (vec (map #(* %1 %2)
                          (v- (:variance ctree) (flat-vector-auto-product (:mean ctree)))
                          (flat-vector-auto-product (get-scale-factors image))))
                (:energy ctree)))

(defn distance-table
  "Given a list of vertices, returns an upper-triangular table that contains pairwise distances."
  [vecs]
  (let [n (count vecs)]
    (for [i (range (dec n))]
      (let [v0 (nth vecs i)]
        (for [j (range (inc i) n)]
          (l2-distance v0 (nth vecs j)))))))

(defn get-distances-from-pivot
  [table j]
  (concat (for [i (range j)]
            (nth (nth table i) (- j i 1)))
          (if (< j (count table)) (nth table j))))

(defn print-distance-table
  [vecs]
  (let [n (count vecs)]
    (dorun (for [i (range (dec n))]
             (let [v0 (nth vecs i)]
               (dorun (for [j (drop 1 (range n))]
                        (if (<= j i)
                          (printf "        ")
                          (printf "%8.3f" (l2-distance v0 (nth vecs j))))))
               (printf "\n"))))))

(defn get-ctree-offsets
  "Returs a set that contains all of the raster offsets contained in the indicated ctree."
  [ct]
  (loop [offsets nil ctrees (list ct)]
    (if (empty? ctrees)
      offsets
      (let [next-tree (first ctrees)]
        (recur (concat offsets (:offsets next-tree)) (concat (:children next-tree) (rest ctrees)))))))

;;; Distance functions
;;; FIXME: This is just an A* search. It should be implemented in a more general and reusable fashion.



(defn distance-box-to-box
  "Returns the Euclidean distance between the bounding boxes defined with respect to the current images.
Note that each bounding box is defined as a pair-vector of raster offsets, e.g. [l1 h1]."
  [image [l1 h1] [l2 h2]]
  (letfn [(projected-distance [a b c d]
                      (square (max 0 (- c b) (- a d))))]
    (Math/sqrt (apply + (apply (partial map projected-distance)
                               (map (partial get-site-of-offset image) [l1 h1 l2 h2]))))))

(defn distance-box-to-offset
  "Computes and returns the minimum Euclidean distance between the indicated bounding box (defined by a pair of
offsets (arg1) and the lattice site referenced by an offset (arg2)."
  [image  [lower upper] offset]
  (let [[lower-site upper-site offset-site] (map #(get-site-of-offset image %) (list lower upper offset))]
    (Math/sqrt (apply + (map #(square (max 0 (- %1 %2) (- %2 %3))) lower-site offset-site upper-site)))))

(defn distance-box-to-offsets
  "Computes and returns the minimum Euclidean distance between a bounding box and the set of raster sites
referenced by a set of raster offsets."
  [image box offsets]
  (letfn [(distance2-boxsites-offset
            [[lo-site hi-site] offset]
            (apply + (map #(square (max 0 (- %1 %2) (- %2 %3)))
                          lo-site (get-site-of-offset image offset) hi-site)))]
          (Math/sqrt (apply min
                 (map (partial distance2-boxsites-offset
                               (map (partial get-site-of-offset image) box))
                      offsets)))))

(defn distance-offsets-to-offsets
  "Computes and returns the minimum Euclidean distance between two sets of lattice sites, indicated
by the corresponding sets of raster offsets."
  [image set1 set2]
  (apply interset-distance
         (map #(map (partial get-site-of-offset image) %)
              [set1 set2])))

(defn distance-offsets-to-offset
  [image set1 offset]
  (Math/sqrt
   (apply min
          (map #(apply square-distance
                       (map (partial get-site-of-offset image)
                            (list offset %))) set1))))

(defn distance-node-to-node
  "Evaluates a lower-bound to the distance between the two indicated ctree nodes. Whenever possible,
the bounding box is used in place of spatial locations of the raster offsets."
  [image ct1 ct2]
  (if (:box ct1)
    (if (:box ct2)
      (distance-box-to-box image (:box ct1) (:box ct2))
      (distance-box-to-offsets image (:box ct1) (:offsets ct2)))
    (if (:box ct2)
      (distance-box-to-offsets image (:box ct2) (:offsets ct1))
      (distance-offsets-to-offsets image (:offsets ct1) (:offsets ct2)))))

(defn distance-node-to-offsets
  "Evaluates a lower-bound to the distance between the two indicated node and the set of
raster offsets."
  [image ct1 offsets2]
  (if (:box ct1)
    (distance-box-to-offsets image (:box ct1) offsets2)
    (distance-offsets-to-offsets image (:offsets ct1) offsets2)))




(defn distance-ctree-offset
  "Computes and returns the minimum Euclidean distance between the indicated ctree (arg2) and offset (arg3)."
  [image ct offset]
  (let [d0 (distance-offsets-to-offset image (:offsets ct) offset)]
    (loop [best-distance d0 candidates (:children ct)]
      (if (empty? candidates)
        best-distance
        (let [next-node (first candidates)
              new-distance (min best-distance (distance-offsets-to-offset image (:offsets next-node) offset))
              good-children (filter #(and % (or (nil? (:box %))
                                                (< (distance-box-to-offset image (:box %) offset) new-distance)))
                                    (:children next-node))]
          (recur new-distance (concat good-children (rest candidates))))))))

(defn distance-ctree-offsets
  "Computes and returns the minimum Euclidean distance between the indicated ctree (arg2) and the lattice
sites that correspond to the set of raster offsets (arg3)."
  [image ct1 set2]
  (apply min (map (partial distance-ctree-offset image ct1) set2)))


(defn lower-distance
  "Returns a lower estimate for the distance between two ctrees."
  [image ct1 ct2]
  (if (:children ct1)
    (if (:children ct2)
      (distance-box-to-box image ct1 ct2)
      (distance-box-to-offsets image ct1 (:offsets ct2)))
    (if (:children ct2)
      (distance-box-to-offsets image ct2 (:offsets ct1))
      (distance-offsets-to-offsets image (:offsets ct1) (:offsets ct2)))
    ))

(defn construct-cross-pairs
  "Generate the next generation of node pairs, estimating the distance between each pair. Only pairs
with estimated distances less that the value of best-distance are retained and returned."
  [image best-distance ct1 ct2]
  (sort-by #(nth % 0)
           (filter #(< (nth % 0) best-distance)
                   (concat (map #(vector (distance-node-to-offsets image % (:offsets ct1))
                                         (assoc ct1 :children nil) %)
                                (:children ct2))
                           (map #(vector (distance-node-to-offsets image % (:offsets ct2))
                                         % (assoc ct2 :children nil))
                                (:children ct1))
                           (for [x1 (:children ct1) x2 (:children ct2)]
                             (vector (distance-node-to-node image x1 x2) x1 x2))))))

(defn distance-ctree-to-ctree
  "Computes and returns the minimum Euclidean distance between two image components referenced by two
component trees (args 2 and 3)."
  [image ct1 ct2]
  ;; First get an initial distance.
  (let [d0 (distance-offsets-to-offsets image (:offsets ct1) (:offsets ct2))]
    (loop [best-distance d0 candidates (construct-cross-pairs image d0 ct1 ct2)]
      (if (empty? candidates)
        best-distance
        (let [next-candidate (first candidates)
              a (nth next-candidate 1)
              b (nth next-candidate 2)
              d1 (distance-offsets-to-offsets image (:offsets a) (:offsets b))
              new-candidates (concat (construct-cross-pairs image d1 a b) (filter #(< (nth % 0) d1) (rest candidates)))]
          (recur d1 (sort-by #(nth % 0) new-candidates)))))))

(defn ctree-distance-bf
  "Computes the distance between two ctrees. This algorithm inefficiently uses brute force, and
eventually computes the distances between every heterogeneous pair of lattice sites."
  [image ct1 ct2]
  (letfn [(get-sites [ct] (map (partial get-site-of-offset image) (get-ctree-offsets ct)))]
    (interset-distance (get-sites ct1) (get-sites ct2))))

(defn ctree-distance-inner
  "Computes the distance between two ctrees, using the bounding-boxes, to branch and bound."
  [image ct1 ct2 best]
  (letfn [(get-site [offset] (get-site-of-offset image offset))
          (get-base-sites [ctree] (map get-site (:offsets ctree)))
          (distance-base-base [b1 b2] (apply interset-distance (map get-base-sites [b1 b2])))
          (distance2-boxsites-offset [[lo-site hi-site] offset]
            (apply + (map #(square (max 0 (- %1 %2) (- %2 %3))) lo-site (get-site offset) hi-site)))
          (distance-base-box [base [lo hi]]
            (Math/sqrt (apply min (map (partial distance2-boxsites-offset (vector (get-site lo) (get-site hi)))
                                       (:offsets base)))))
          (distance-box-box [[l1 h1] [l2 h2]]
            (letfn [(projected-distance [a b c d]
                      (square (max 0 (- c b) (- a d))))]
              (Math/sqrt (apply + (apply (partial map projected-distance)
                                          (map get-site [l1 h1 l2 h2]))))))
          ]
    (loop [best best subtrees (list ct2)]
      (if (empty? subtrees)
        best
        (let [a1 (first subtrees)]
          (if (< best (if (:box a1) (distance-box-box (:box ct1) (:box a1)) (distance-base-box a1 (:box ct1))))
            (recur best (rest subtrees))
            (recur (min best (distance-base-base ct1 a1)) (concat (:children a1) (rest subtrees)))))))
    
    #_(vector (distance-base-base ct1 ct2)
            (distance-base-box ct1 (:box ct2))
            (distance-base-box ct2 (:box ct1))
            (distance-box-box (:box ct1) (:box ct2)))

    ))

(defn ctree-distance
  [image ct1 ct2]
  (loop [best 100000000 subtrees (list ct2)]
    (if (empty? subtrees)
      best
      (let [a1 (first subtrees)
            d1 (ctree-distance-inner image a1 ct1 best)]
        (if (< best d1)
          (recur best (rest subtrees))
          (recur d1 (concat (:children a1) (rest subtrees))))))))


(defn box-distance
  [image ct1 ct2]
  (letfn [(get-site [offset] (get-site-of-offset image offset))
          (projected-distance [a b c d]
            (square (max 0 (- c b) (- a d))))]
    (Math/sqrt (apply + (apply (partial map projected-distance)
                               (map get-site (mapcat :box [ct1 ct2])))))))

(defn root-distance
  [image ct1 ct2]
  (letfn [(get-site [offset] (get-site-of-offset image offset))]
    (apply interset-distance (map :offsets [ct1 ct2]))))

(defn bb-distance
  "Uses a branch and bound heuristic to compute the Euclidean distance between the components
rooted at ComponentTreeNodes ct1 and ct2."
  [image ct1 ct2]
  (letfn [(node-distance [a b] (root-distance image a b)) ]
  (let [d0 (node-distance ct1 ct2)
        [ch1 ch2] (vec (map :children [ct1 ct2]))
        pairs (sort-by first <
                       (filter #(< (first %) d0)
                               (for [x ch1 y ch2]
                                 (vector (box-distance x y) (vector x y)))))]
    (loop [d d0 candidates pairs]
      (if (empty? candidates)
        d
        (let [greedy-pick (first candidates)
              d1 (apply node-distance (second greedy-pick))]
          (if (< d1 d)
            (recur d1 (concat (rest candidates))))))))))
;(debug :render-ctree)

(defn render-ctree-2d
  "Displays the 2d image (arg1) in a JFrame GUI, using a grayscale, along with each
component tree in the list ctrees (arg2), which are each rendered using an index
color."
  [{:keys [dimensions raster] :as image} ctrees roi]
  (let [max-side 1024
        frame (JFrame. "Component Viewer")
        [[col0 row0] [coln rown]] (map (partial get-site-of-offset image) roi)
        cols (- coln col0)
        rows (- rown row0)
        scale (min (/ max-side (max cols rows)) 10)
        buffer (new BufferedImage
                    (* scale cols)
                    (* scale rows)
                    BufferedImage/TYPE_INT_ARGB)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g buffer 0 0 this)))
        graphics (.createGraphics buffer)]
    (doseq [x (range cols)
            y (range rows)]
      (let [v (get-pixel-site image (v+ (vector x y) (vector col0 row0)))]
        (doto graphics
          (.setColor (new Color v v v 255))
          (.fillRect (* x scale) (* y scale) scale scale))))
    
    (doseq [ct (zipmap (iterate inc 0) ctrees)]
      (let [color-index (first ct)]
        (loop [ct (list (second ct))]
          (when (not (empty? ct))
            (let [current (first ct)
                  intensity (:intensity current)
                  offsets (:offsets current)
                  children (:children current)]
              (dbg :render-ctree  "intensity-> ~a, offsets-> ~a~%" intensity offsets)
              (doseq [o (seq offsets)]
                (let [[x y] (get-site-of-offset image o)
                      [r g b] (get-indexed-rgb color-index)]
                  (dbg :render-ctree  "   [x y] = [~a ~a]~%" x y)
                  (doto graphics
                    (.setColor (new Color r g b 144))
                    (.fillRect (* (- x col0) scale) (* (- y row0) scale) scale scale))))
              (recur (concat children (rest ct))))))))
    
    (.add frame canvas)
    (.setSize frame (new Dimension (* scale cols) (+ 20 (* scale rows))))
    (.show frame))
  )


(defn render-ctree-3d
  "Displays the 3d image (arg1) in a JFrame GUI, using a grayscale, along with each
component tree in the list ctrees (arg2), which are each rendered using an index
color."
  [{:keys [dimensions] :as image} ctrees roi]
  (let [max-side 1024
        frame (JFrame. "Component Viewer")
        [site0 siten] (map (partial get-site-of-offset image) roi)
        [cols rows lays] (v- siten site0)
        scale (int (min (/ max-side (max cols rows)) 20))
        buffer (new BufferedImage
                    (* scale cols)
                    (* scale rows)
                    BufferedImage/TYPE_INT_ARGB)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g buffer 0 0 this)))
        graphics (.createGraphics buffer)]
    (doseq [x (range cols)
            y (range rows)]
      (let [v (for [z (range lays)] (get-pixel-site image (v+ (vector x y z) site0)))
            vm  (apply max v)]
        (doto graphics
          (.setColor (new Color vm vm vm 255))
          (.fillRect (* x scale) (* y scale) scale scale))))
    
    (doseq [ct (zipmap (iterate inc 0) ctrees)]
      (let [color-index (first ct)]
        (loop [ct (list (second ct))]
          (when (not (empty? ct))
            (let [current (first ct)
                  intensity (:intensity current)
                  offsets (:offsets current)
                  children (:children current)]
              (dbg :render-ctree  "intensity-> ~a, offsets-> ~a~%" intensity offsets)
              (doseq [o (seq offsets)]
                (let [[x y _] (get-site-of-offset image o)
                      [r g b] (get-indexed-rgb color-index)]
                  (dbg :render-ctree  "   [x y] = [~a ~a]~%" x y)
                  (doto graphics
                    (.setColor (new Color r g b 144))
                    (.fillRect (* (- x (first site0)) scale)
                               (* (- y (second site0)) scale) scale scale))))
              (recur (concat children (rest ct))))))))
    
    (.add frame canvas)
    (.setSize frame (new Dimension (* scale cols) (+ 20 (* scale rows))))
    (.show frame))
  )

(defn create-buffered-image-of-ctree-3d
  "Displays the 3d image (arg1) in a JFrame GUI, using a grayscale, along with each
component tree in the list ctrees (arg2), which are each rendered using an index
color."
  [{:keys [dimensions] :as image} ctrees roi]
  (let [[site0 siten] (map (partial get-site-of-offset image) roi)
        [cols rows lays] (v- siten site0)
        buffer (new BufferedImage cols rows BufferedImage/TYPE_INT_ARGB)
        graphics (.createGraphics buffer)]
    (doseq [x (range cols)
            y (range rows)]
      (let [v (for [z (range lays)] (get-pixel-site image (v+ (vector x y z) site0)))
            vm  (apply max v)]
        (doto graphics
          (.setColor (new Color vm vm vm 255))
          (.fillRect x y 1 1))))
    
    (doseq [ct (zipmap (iterate inc 0) ctrees)]
      (let [color-index (first ct)]
        (loop [ct (list (second ct))]
          (when (not (empty? ct))
            (let [current (first ct)
                  intensity (:intensity current)
                  offsets (:offsets current)
                  children (:children current)]
              (dbg :render-ctree  "intensity-> ~a, offsets-> ~a~%" intensity offsets)
              (doseq [o (seq offsets)]
                (let [[x y _] (get-site-of-offset image o)
                      [r g b] (get-indexed-rgb color-index)]
                  (dbg :render-ctree  "   [x y] = [~a ~a]~%" x y)
                  (doto graphics
                    (.setColor (new Color r g b 144))
                    (.fillRect  (- x (first site0)) (- y (second site0)) 1 1))))
              (recur (concat children (rest ct))))))))
    buffer))

(defn display-ctree-3d
  [image title ctrees roi]
  (let [[site0 siten] (map (partial get-site-of-offset image) roi)
        [cols rows lays] (v- siten site0)
        frame (JFrame. title)
        buffer (create-buffered-image-of-ctree-3d image ctrees roi)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g buffer 0 0 this)))]
    (.add frame canvas)
    (.setSize frame (new Dimension  cols (+ 20 rows)))
    (.show frame)))

(defn export-rendered-ctree-3d
  "Generate a png image file of the indicated image with the superimposed component tree."
  [filename image ctrees roi]
  (let [image-writer (.next (ImageIO/getImageWritersByFormatName "png"))
        buffer (create-buffered-image-of-ctree-3d image ctrees roi)
        ;; file (File. filename)
        ]
    (try
      (with-open [image-output-stream (ImageIO/createImageOutputStream
                                       (clojure.java.io/file filename))]
      (.setOutput image-writer image-output-stream)
      (.write image-writer buffer))
      ;;  (ImageIO/write buffer "PNG" file)
      (finally (do (.dispose image-writer))))))

;;; Tests

(deftest blobify.image-test
  (def img2d (blobify.image/make-blobby-image-2d 16))
  (def img3d (blobify.image/make-blobby-image-3d 8)))
