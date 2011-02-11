;;; The implementation of the component tree for gray-level images.

(ns clj-ctree.core
  (:use  clj-ctree.vectors
		 clj-ctree.polynomial
		 clj-ctree.image
		 clj-ctree.utils				; dbg, seq2redundant-map
		 clojure.set
		 clojure.contrib.pprint
		 ))

(defn bin-by-intensity
  "Given an image (arg1) bin-by-intensity returns an intensity map of the image, in which
each intensity value is represented by a key (sorted in decreasing order), followed by a list
of offsets corresponding to each intensity value."
  [image]
  (letfn [(key-value-pair [x] (vector (get-pixel image x) x))]
	(seq2redundant-map (range (get-size image)) key-value-pair conj :sort-down)))

; (debug :make-ctree-gbn) ; remove/insert leading comment to toggle debugging
; (debug :make-ctree-cc) ; remove/insert leading comment to toggle debugging

(defn make-ctree-as-map-of-unions
  "Given an image (arg1) and a maximum Hamming radius (arg2), make-ctree-as-a-map-of-unions
generates and returns a component tree for the image, consisting of a map with integer keys,
corresponding to a decreasing sequence of image intensities, with corresponding values
consisting of lists of sets of the topologically connected components with intensities
equal to or greater than each intensity threshold."
  ([image max-hamming-radius min-intensity]
	 (let [bins (filter #(>= (first %) min-intensity) (bin-by-intensity image))
		   kernel (get-neighborhood-mask (get-dimensionality image) max-hamming-radius)
		   k-values (keys bins)]
	   (letfn [(get-bright-neighbors	; return a list of offsets with intensities that are greater
				[offset]				; than or equal to that of the indicated offset (arg1).
				(let [threshold (get-pixel image offset)
					  value (get-filtered-neighborhood image kernel #(<= threshold %1) offset)]
				  (dbg :make-ctree-gbn "(get-bright-neighbors ~a) => ~s~%" offset value)
				  value))
			   
			   ;; In the following, a component is implemented as a set. Components that include 
			   ;; pixels with intensities greater than or equal to that of the indicated offset (arg2)
			   ;; will be collected and returned as a list of sets. The function begins with a list
			   ;; of components, called active-sets (arg1), that are defined for the minimum pixel
			   ;; intensity greater than that of offset (arg2). (For the initial iteration, active-sets
			   ;; is set to nil.) The main body of make-ctree uses reduce to merge components that
			   ;; become topologically adjacent as the threshold is decreased. collect-components
			   ;; scans the list of active sets, merging those that contain a topological neighbor of
			   ;; offset, and returns the result, as a list of sets.
			   (collect-components
				[active-sets offset]
				(let [neighbors (into #{} (get-bright-neighbors offset))]
				  (dbg :make-ctree-cc "active-sets-> ~a, offset-> ~a:~%" active-sets offset)
				  ;; In the loop, current-set is the component that contains offset. The other-sets
				  ;; is a list of components (taken from open-sets) that are not adjacent to offset.
				  (loop [open-sets active-sets current-set (into #{} (list offset)) other-sets ()]
					(if (empty? open-sets)
					  (let [value (conj other-sets current-set)]
						(dbg-indent :make-ctree-cc 1 "returning ~a~%" value)
						value)
					  (let [s (first open-sets),
							[new-current-set new-other-sets] (if (empty? (intersection neighbors s))
															   (vector current-set (conj other-sets s))
															   (vector (union s current-set) other-sets))]
						(dbg-indent :make-ctree-cc 1 "(intersection ~a ~a) => ~a"
									neighbors s (intersection neighbors s))
						(dbg-indent :make-ctree-cc 1 "s-> ~a, new-current-set-> ~a, new-other-sets -> ~a~%"
									s new-current-set new-other-sets)
						(recur (rest open-sets) new-current-set new-other-sets))))))]
		 (loop [ctree (sorted-map-by #(compare %2 %1)) b bins]
		   (if (empty? b)
			 ctree
			 (let [[k offsets] (first b),
				   k-sets (reduce #(collect-components %1 %2)
								  (get ctree (least-above k k-values))
								  offsets)]
			   (recur (merge ctree (into {} (list (vector k k-sets)))) (rest b))))))))
  ([image max-hamming-radius] (make-ctree-as-map-of-unions image max-hamming-radius 0)))

(defn analyze-ctree-as-map-of-unions
  "Given a component tree, ctree (arg1), as generated by make-ctree-as-map-of-unions, analyze-ctree
generates a new map, with integer-valued keys, corresponding the intensities of the component tree,
followed by a list of integers that correspond to the size of each component."
  [ctree]
  (into (sorted-map-by #(compare %2 %1))
		(for [[k v] ctree]
		  (vector k (map count v)))))

;;; In the following, we try a more concise implementation in which each node in the component tree
;;; is implemented as a struct. The intensity should be an integer corresponding to the monochrome
;;; pixel intensity of the pixels that are included with this node. The offsets is a set 
;;; of these pixels in the current raster vector. 

(defstruct ctree :intensity :offsets :children)

;; TODO: change defstruct to defrecord:
;; (defrecord ctree [intensity offsets children])
;;

(defn ctree-contains-offset?
  [ctree intensity offset]
  (cond (empty? ctree) false,
		(< intensity (:intensity ctree)) false,
		(= intensity (:intensity ctree)) (contains? (:offsets ctree) offset),
		:else (some #(ctree-contains-offset? % intensity offset) (:children ctree))))

(defn merge-ctrees
  "Creates a new ctree created by merging the two indicated ctrees, indicated that the components
represented by each unite at the minimum intensity of the two. Note that the function is symmetric."
  [ct1 ct2]
  (let [{i1 :intensity s1 :offsets ch1 :children} ct1
		{i2 :intensity s2 :offsets ch2 :children} ct2]
	(cond (< i1 i2) (struct ctree i1 s1 (conj ch1 ct2)),
		  (= i1 i2) (struct ctree i1 (union s1 s2) (concat ch1 ch2)),
		  :else (struct ctree i2 s2 (conj ch2 ct1)))))

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
  "Given an image structure (arg1), an integer hamming radius (arg2), and a minimum intensity
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
				(let [neighbors (get-bright-neighbors offset)]
				  (dbg :make-ctree-cc "active-sets-> ~a, offset-> ~a:~%" ctrees offset)
				  ;; In the loop, current-set is the component that contains offset. The other-sets
				  ;; is a list of components (taken from open-sets) that are not adjacent to offset.
				  (loop [open-roots ctrees,
						 local-root (struct ctree (get-pixel image offset) (set (list offset)) nil),
						 other-roots nil]
					(if (empty? open-roots)
					  (let [value (conj other-roots local-root)]
						(dbg-indent :make-ctree-cc 1 "returning ~a~%" value)
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

(defn chop
  [ctrees threshold]
  (letfn [(filter-fn [ctree] (<= threshold (:intensity ctree)))
		  (children-fn [ctree] (:children ctree))]
	(tree-select filter-fn children-fn ctrees)))
