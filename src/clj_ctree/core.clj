;;; The implementation of the component tree for gray-level images.

(ns clj-ctree.core
  (:import (java.awt.image BufferedImage)
           (java.awt Color Graphics Dimension)
           (javax.swing JPanel JFrame JLabel)
           (ij ImagePlus)
           (org.imagearchive.lsm.reader Reader))
  (:use  [clj-ctree.image :only (get-dimensionality
								 get-filtered-neighborhood
								 get-neighborhood-mask
								 get-pixel
                         get-pixel-site
                         get-site-of-offset
                         get-position-of-site
								 get-size
								 with-image-get-neighboring-offsets)]
         [clj-ctree.utils :only (dbg
 								 dbg-indent
                         debug
								 least-above
								 seq2redundant-map
                         tree-select
                         when-dbg)]
         [clj-ctree.vectors :only (l2-distance
                                   vector-sub
                                   vector-interp
                                   vector-square)]
         [clj-ctree.graphics :only (get-indexed-rgb)]
		 [clojure.set :only (intersection
							 union)]
		 [clojure.contrib.pprint :only (cl-format)]
       clojure.test
		 ))

(defn bin-by-intensity
  "Given an image (arg1) bin-by-intensity returns an intensity map of the image, in which
each intensity value is represented by a key (sorted in decreasing order), followed by a list
of offsets corresponding to each intensity value."
  [image]
  (letfn [(key-value-pair [x] (vector (get-pixel image x) x))]
	(seq2redundant-map (range (get-size image)) key-value-pair conj :sort-down)))

;; (debug :make-ctree-gbn) ; remove/insert leading comment to toggle debugging
;; (debug :make-ctree-cc) ; remove/insert leading comment to toggle debugging

;;; In the following, we try a more concise implementation in which each node in the component tree
;;; is implemented as a struct. The intensity should be an integer corresponding to the monochrome
;;; pixel intensity of the pixels that are included with this node. The offsets is a set 
;;; of these pixels in the current raster vector. 

(defrecord ComponentTreeNode [intensity offsets size mean variance energy children])

(defn ctree-contains-offset?
  "Returns true if the ctree contains the indicated intensity-offset pair"
  [ctree intensity offset]
  (cond (empty? ctree) false,
		(< intensity (:intensity ctree)) false,
		(= intensity (:intensity ctree)) (contains? (:offsets ctree) offset),
		:else (some #(ctree-contains-offset? % intensity offset) (:children ctree))))

(defn merge-ctrees
  "Creates a new ctree created by merging the two indicated ctrees, indicated that the components
represented by each unite at the minimum intensity of the two. Note that the function is symmetric."
  ;; [ct1 ct2]
  [{i1 :intensity set1 :offsets n1 :size m1 :mean v1 :variance e1 :energy ch1 :children :as ct1}
   {i2 :intensity set2 :offsets n2 :size m2 :mean v2 :variance e2 :energy ch2 :children :as ct2}]
  (dbg :merge-ctrees "~&ct1-> ~a~% ct2-> ~a~%~%" (:size ct1) (:size ct2))
  (let [
        ;{i1 :intensity set1 :offsets n1 :size m1 :mean v1 :variance e1 :energy ch1 :children} ct1
        ;{i2 :intensity set2 :offsets n2 :size m2 :mean v2 :variance e2 :energy ch2 :children} ct2
        n12 (+ n1 n2)
        r12 (float (/ n1 n12)) ; interpolation parameter
        m12 (vector-interp m2 m1 r12) ; r12 = 0 implies m12 = m2
        v12 (/ (+ (* n1 (+ v1 (vector-square (vector-sub m1 m12))))
                  (* n2 (+ v2 (vector-square (vector-sub m2 m12))))) n12)
        e12 (+ e1 e2)]
	(cond (< i1 i2) (ComponentTreeNode. i1 set1 n12 m12 v12 e12 (conj ch1 ct2)),
         (= i1 i2) (ComponentTreeNode. i1 (union set1 set2)  n12 m12 v12 e12 (concat ch1 ch2)),
         :else (ComponentTreeNode. i2 set2 n12 m12 v12 e12 (conj ch2 ct1)))))


(defn pprint-ctree
  ([stm level {:keys [intensity offsets children] :as ctree}]
	 (let [indentation (apply str (take (* 3 level) (repeat \space)))]
	   (if (empty? ctree)
		 (cl-format stm "~&~a []" indentation),
		 (dorun (cl-format stm "~&~a[~a, ~a:~%" indentation intensity offsets)
				(dorun (map #(pprint-ctree stm (inc level) %) children))))))
  ([stm ctree] (pprint-ctree stm 0 ctree))
  ([ctree] (pprint-ctree true 0 ctree)))

(defn make-ctree
  "Given an Image (arg1), an integer hamming radius (arg2), and a minimum intensity
threhsold, make-ctree generates a component tree for the image assuming the topology specified
by the second argument (adjacent lattice sites within the hamming radius are topologically
connected). Pixel intensities below min-intensity are ignored."
  ([image max-hamming-radius min-intensity]
     (let [bins (filter #(>= (first %) min-intensity) (bin-by-intensity image))
           mask (get-neighborhood-mask (get-dimensionality image) max-hamming-radius)
           k-values (keys bins)]
       (letfn [(get-bright-neighbors	; return a list of offsets with intensities that are greater
                [offset]				; than or equal to that of the indicated offset (arg1).
                (let [threshold (get-pixel image offset)
                      neighbors (map #(vector (get-pixel image %) %)
                                     (with-image-get-neighboring-offsets image mask offset))
                      brights (filter #(<= threshold (first %)) neighbors)]
                  (dbg :make-ctree-gbn "(get-bright-neighbors ~a) => ~s~%" offset brights)
                  brights))
			   
               ;; In the following, a component is implemented as a ctree. Components that include 
               ;; pixels with intensities greater than or equal to that of the indicated offset (arg2)
               ;; will be collected and returned as a list of ctree-nodes. The function begins with a list
               ;; of components, called ctrees (arg1), that are defined for the minimum pixel
               ;; intensity greater than that of offset (arg2). (For the initial iteration, ctrees (arg1)
               ;; is set to nil.) The main body of make-ctree uses reduce to merge components that
               ;; become topologically adjacent as the threshold is decreased. collect-compoonents
               ;; scans the list of active ctrees, merging those that contain a topological neighbor of
               ;; the current offset, and returns the result, as a list of ctrees.
               (collect-components
                [ctrees offset]
                (let [neighbors (get-bright-neighbors offset)
                      intensity (get-pixel image offset)
                      mean (get-position-of-site image (get-site-of-offset image offset))]
                  (dbg :make-ctree-cc "active-sets-> ~a, offset-> ~a:~%" ctrees offset)
                  ;; In the loop, current-set is the component that contains offset. The other-sets
                  ;; is a list of components (taken from open-sets) that are not adjacent to offset.
                  (loop [open-roots ctrees,
                         local-root (ComponentTreeNode. intensity
                                                        (set (list offset))
                                                        1 
                                                        mean
                                                        0.0 
                                                        intensity
                                                        nil),
                         
                         other-roots nil]
                    (if (empty? open-roots)
                      (let [value (conj other-roots local-root)]  ; here the let facilitates the following side effect.
                        (dbg-indent :make-ctree-cc 1 "returning ~a ~%" value)
                        value)
                      (let [s (first open-roots)]
                        (if (some #(ctree-contains-offset? s (first %) (second %)) neighbors )
                          (do (dbg :make-ctree-cc "offset ~a adjoins  ~s. (neighbors= ~a)~%" offset s neighbors)
                              (recur (rest open-roots) (merge-ctrees local-root s) other-roots))
                          (do (dbg :make-ctree-cc "offset ~a disjoins ~s. (neighbors= ~a)~%" offset s neighbors)
                              (recur (rest open-roots) local-root (conj other-roots s)))))))))]
         (loop [ct nil b bins]
           (when-dbg :make-ctree (dorun (map #(pprint-ctree %) ct)))
           (if (empty? b)
             ct
             (let [[k offsets] (first b),
                   k-sets (reduce #(collect-components %1 %2)
                                  ct
                                  offsets)]
               (recur k-sets (rest b)))))))))

(defn chop-intensity
  [ctrees threshold]
  (letfn [(filter-fn [ctree] (<= threshold (:intensity ctree)))
		  (children-fn [ctree] (:children ctree))]
	(tree-select filter-fn children-fn ctrees)))

(defn chop-size
  [ctrees threshold]
  (letfn [(filter-fn [ctree] (>= threshold (:size ctree)))
		  (children-fn [ctree] (:children ctree))]
	(tree-select filter-fn children-fn ctrees)))

;;; Functions that anayze a list of ctrees.

(defrecord NodeSummary [size mean variance energy])

(defn make-summary
  [ctree]
  (NodeSummary. (:size ctree) (:mean ctree) (:variance ctree) (:energy ctree)))

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

;(debug :render-ctree)
  

(defn render-ctree-2d
  "Displays the 2d image (arg1) in a JFrame GUI, using a grayscale, along with each
component tree in the list ctrees (arg2), which are each rendered using an index
color."
  [{:keys [dimensions raster] :as image} ctrees]
  (let [max-side 1024
        frame (JFrame. "Component Viewer")
        cols (dimensions 0)
        rows (dimensions 1)
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
      (let [v (get-pixel-site image (vector x y))]
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
                    (.fillRect (* x scale) (* y scale) scale scale))))
              (recur (concat children (rest ct))))))))
    
    (.add frame canvas)
    (.setSize frame (new Dimension (* scale cols) (+ 20 (* scale rows))))
    (.show frame))
  )


(defn render-ctree-3d
  "Displays the 3d image (arg1) in a JFrame GUI, using a grayscale, along with each
component tree in the list ctrees (arg2), which are each rendered using an index
color."
  [{:keys [dimensions] :as image} ctrees]
  (let [max-side 1024
        frame (JFrame. "Component Viewer")
        cols (dimensions 0)
        rows (dimensions 1)
        layers (dimensions 2)
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
      (let [v (for [z (range layers)] (get-pixel-site image (vector x y z)))
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
                    (.fillRect (* x scale) (* y scale) scale scale))))
              (recur (concat children (rest ct))))))))
    
    (.add frame canvas)
    (.setSize frame (new Dimension (* scale cols) (+ 20 (* scale rows))))
    (.show frame))
  )

;;; Tests

(deftest clj-ctree.image-test
  (def img2d (clj-ctree.image/make-blobby-image-2d 16))
  (def img3d (clj-ctree.image/make-blobby-image-3d 8)))
