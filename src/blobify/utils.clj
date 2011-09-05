;;; utils.clj is part of the source code for blobify.

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
;;; Contains functions that extend the power of clojure, for peforming a
;;; variety of tasks:
;;;
;;; Type conversion
;;; * int-to-ubyte
;;; * ubyte-ti-int
;;;
;;; Seq construction
;;; * seq2map
;;; * seq2redundant-map
;;;
;;; Numerical evaluation
;;; * least-above
;;; * greatest-below
;;; * absolutely-within
;;; * relative-difference
;;; * pos-if
;;; * pos-if*
;;; * pos-value
;;; * pos-value*
;;; * floor-rem
;;; * square
;;;
;;; Sequence Operations
;;; * get-index
;;; * get-index-list
;;; * get-last-index
;;; * shift-seq-left
;;; * shift-seq-right


;;;
;;; Debugging utilities
;;; * debug
;;; * undebug
;;; * get-debug-ids
;;; * set-debug-stream
;;; * get-debug-stream
;;; * when-dbg
;;; * dbg
;;; * dbg-indent
;;; * dbg-reset
;;;
;;; * tree-search
;;;
;;; String Operations
;;; * get-directory
;;; * get-filename
;;; * get-filename-ext
;;; * get-filename-root
;;;

(ns blobify.utils
  (:use [clojure.set :only (difference union)]
		[clojure.contrib.pprint]))

;;;
;;; type conversion
;;;

(defn int-to-ubyte
  "Converts the integer x (arg1) in the range [0, 255] to a byte. Note that bytes in clojure and java are
regrettably always signed."
  [ x ]
  {:pre [(<= 0 x 255)]}
  (byte (if (< x 128) x (- x 256))))

(defn ubyte-to-int
  "Converts the unsigned byte x (arg1), which should be in the range [-128, 127] to a non-negative
integer, in the interval [0, 255]"
  [ x ]
  {:pre [(<= -128 x 127)]}
  (let [y (int x)]
	(if (<= 0 y) y (+ 256 y))))

;;; contributed by Garw W. Johnson, Jr.:
(defn seq2map
  "Constructs a map from a sequence by applying keyvalfn to each
   element of the sequence.  keyvalfn should return a pair [key val]
   to be added to the map for each input sequence element."
  [aseq keyvalfn]
  (into {} (map keyvalfn aseq)))

;;; contributed by Garw W. Johnson, Jr.:
(defn seq2redundant-map
  "Constructs a map from a sequence by applying keyvalfn to each
   element of the sequence.  keyvalfn should return a pair [key val]
   to be added to the map for each input sequence element.  If key is
   already in the map, its current value will be combined with the new
   val using (mergefn curval val). An optional fourth argument defines
   the type of map that is assumed, chosen from the keywords
   :unsorted (for a hash-map, default), :sort-up (for a sorted-map
   in increasing key order), and :sort-down (for a sorted-map in
   decreasing key order)."
  ([aseq keyvalfn mergefn map-type]
	 {:pre [(contains? #{:hash :sort-up :sort-down} map-type)]}
	 (let [initial-map ({:hash (hash-map),
						 :sort-up (sorted-map),
						 :sort-down (sorted-map-by #(compare %2 %1))} map-type)]
	   (reduce (fn [amap x]
				 (let [[key val] (keyvalfn x)]
				   (update-in amap [key] mergefn val)))
			   initial-map
			   aseq)))
 ([aseq keyvalfn mergefn]
	(seq2redundant-map aseq keyvalfn mergefn :hash)))

;; Numeric functions
(defn least-above
  "Returns the minimum value in coll (arg2) that is greater than x (arg1)."
  [x coll]
  (let [above-x (filter #(< x %) coll)]
	(if (empty? above-x)
	  nil
	  (apply min above-x))))

(defn greatest-below
  "Returns the maximum value in coll (arg2) that is less than x (arg1)."
  [x coll]
  (let [below-x (filter #(< % x) coll)]
	(if (empty? below-x)
	  nil
	  (apply max (filter #(< % x) coll)))))

(defn absolutely-within
  "Returns true if and only if the absolute difference between x and y is within eps.
   Otherwise, false is returned. See also relatively-epsilon."
  [eps x y]
  (< (Math/abs (- x y)) eps))

(defn relative-difference
  "Returns the relative differecent betweeen x and y, namely,
    (x - y)/y. And exception occurs if y is zero."
  [x y]
  {:pre [(not (== y 0))]}
  (/ (- x y) y))

(defn relatively-within
  "Returns true if and only if the relative difference between x and y, namely,
      (x - y)/y
   is within eps. Otherwise false is returned. An esception occurs if y is zero."
  [eps x y]
  (< (Math/abs (relative-difference x y)) eps))

;;;

(defn pos-if
  "Given a predicate pred? and a sequence seq, returns the integer offset of the
   first item in seq that is in the truth set of pred?."
	 [pred? seq]
	 (loop [s seq i 0]
	   (cond (empty? s) nil
			 (pred? (first s)) i
			 :else (recur (rest s) (inc i)))))

(defn pos-if*
  "Given a predicate pred? and a sequence seq, returns a vector of integer offsets
   of every item in seq that is in the truth set of pred?."
  [pred? seq]
  (loop [s seq i 0 output []]
	(cond (empty? s) output
		  (pred? (first s)) (recur (rest s) (inc i) (conj output i))
		  :else (recur (rest s) (inc i) output))))

(defn pos-value
  "Given a value val, and a sequence seq, returns the integer offset of the
   first item in seq that equals val using =."
  [val seq]
  (pos-if (partial = val) seq))

(defn pos-value*
  "Given a value val, and a sequence seq, returns a vector of integer offsets
   of every item in seq that equals val using =."
  [val seq]
  (pos-if* (partial = val) seq))

(defn get-index
  "Given a sequence seq and value val, returns the first integer position in the
   sequence that matches val. Returns nil if no matches are found."
  [val seq]
  (loop [s seq i 0]
	(cond (empty? s) nil
	       (= (first s) val) i
	       :else (recur (rest s) (inc i)))))

(defn get-index-list
  "Given a sequence seq and value val, returns a list of all integer positions
   in the sequence that matches val. An empty list is returned if no matches
   are found."
  [val s]
  (loop [s s i 0 output []]
	(cond (empty? s) (seq output)
		  (= (first s) val) (recur (rest s) (inc i) (conj output i))
		  :else (recur (rest s) (inc i) output))))

(defn get-last-index
  [val s]
  (loop [s s i 0 index nil]
    (cond (empty? s) index
          (= (first s) val) (recur (rest s) (inc i) i)
          :else (recur (rest s) (inc i) index))))


(defn shift-seq-left
  [seq]
  (if (empty? seq)
	()
	(concat (rest seq) (list (first seq)))))

(defn shift-seq-right
  [seq]
  (if (empty? seq)
	()
	(let [size (count seq)]
	  (concat (list (last seq)) (take (dec size) seq)))))

;;; debugging tools insipired by Peter Norvig, from "Paradigms of Artificial Intelligence Programming"
;;; Morgan-Kaufmann, San Francisco, 1992, pages 123--124.

(let [dbg-ids (ref #{})
	  dbg-stream (ref true)]

  (defn debug
	"Add one or more ids to the list of active debug keys. See also functions undebug and dbg
dbg-indent, debug-reset, and when-dbg."
	[& ids]
	(dosync (alter dbg-ids union (set ids))))
  
  (defn undebug
	"Remove one or more ids from the list of active debug keys. See also functions debug and dbg."
	[& ids]
	(dosync (alter dbg-ids difference (set ids))))

  (defn get-debug-ids
	"Returns the list of active debug keys. Useful for debugging debug and undebug. See also debug and undebug."
	[]
	@dbg-ids)

  (defn set-debug-stream
	"Set the debug stream (used by cl-format) for printing debugging messages. See also function dbg."
	[s]
	(dosync (ref-set dbg-stream s)))

  (defn get-debug-stream
	"Return the value of the debug stream"
	[]
	@dbg-stream)

;;; TODO: Test when-dbg more thoroughly
  ;; (defmacro when-dbg
  ;; 	[id & body]
  ;; 	(list 'if (contains? @dbg-ids id)
  ;; 	  (cons 'do body)))

  ;; The macro versions of these commands assume the state of dbg-ids at the time of compilation. This is not
  ;; so useful.

(defmacro when-dbg
  [id & body]
  `(if (contains? ~(deref dbg-ids) ~id)
	 (do ~@body)))

  
;;; TODO: Figure out why the following does not work.
;;   (defmacro dbg
;; 	"If the indicated debug id (arg1) is active, then the provided format-string (arg2),
;; which may refer to optional additional arguments (arg3+) is printed to the current debug
;; stream using cl-format."
;; 	[id format-string & args]
;; 	`(when-dbg ~id
;; 			  ((partial cl-format ~(deref dbg-stream) ~format-string) ~@args)))

(defn dbg
  "If the indicated debug id (arg1) is active, then the provided format-string (arg2),
which may refer to optional additional arguments (arg3+) is printed to the current debug
stream using cl-format. See also dbg-indent, debug, debug-reset, and when-dbg."
  [id format-string & args]
  (when (contains? @dbg-ids id)
	(apply (partial cl-format @dbg-stream format-string) args)))

(defn dbg-indent
  "If the indicated debug id (arg1) is active, then the provided format-string (arg3),
which may refer to optional additional arguments (arg4+) is printed to the current debug
stream using cl-format with an indentation level of n (arg2). See also dbg, debug,
debug-rest, and when-dbg."
  [id n format-string & args]
  (when (contains? @dbg-ids id)
	(let [fs (apply str (concat "~&" (take (* 3 n) (repeat \space)) format-string))]
	  (apply (partial cl-format @dbg-stream fs) args))))

;; (defmacro dbg-indent
;; [id n format-string & args]
;; `(let [fmt-string# (apply str (concat "~&" (take (* 3 ~n) (repeat \space)) ~format-string))]
;;    (when-dbg ~id ((partial cl-format ~(deref dbg-stream) fmt-string#) ~@args))))
  
  (defn debug-reset
	"Clears the list of debug ids, and resets the debug stream to true (stdio)."
	[]
	(dosync (ref-set dbg-ids #{})
			(ref-set dbg-stream true)))
  ) 

;;;
;;; TODO: Create a test for this function:
;;;
(defn tree-select
  "Given a node-test predicate, a node-children accessor function (arg2) and a list of trees (arg3),
tree-select navigates up each tree, and returns a list of nodes that satisfy the node-test. Note
that if a node passes the test, then its children are not tested. Thus we seek the set of shallowest
nodes (those closest to the root) within each lineage branch that sastifies the node test.
With each recursive iteration, a node is tested. If it passes the test, then it is added to the
list of hits, which will eventually be returned. Note that if any node passes the test, then
its children are ignored."
  [node-test? get-children-fn list-of-trees]
  (loop [candidates list-of-trees hits ()]
	(if (empty? candidates)
	  hits
	  (let [subject (first candidates)]
		(if (node-test? subject)
		  (recur (rest candidates) (conj hits subject))
		  (recur (concat (get-children-fn subject) (rest candidates)) hits))))))


;;; string operations
(defn get-directory
  "Given a complete path string (arg1), returns a string that contains just the directory."
  [path]
  (let [sep (java.io.File/separatorChar)
        sep-index (get-last-index sep (seq path))]
    (if sep-index
      (subs path 0 (inc sep-index))  ; increment index to include the separation character.
      "")))

(defn get-filename
  "Given a complete path string (arg1), returns a string that contains the filenemae (with extension)."
  [path]
  (let [sep (java.io.File/separatorChar)
        sep-index (get-last-index sep (seq path))]
    (if sep-index
      (subs path (inc sep-index))
      path)))

(defn get-filename-ext
  "Given a complete path string (arg1), returns a string that contains just the file extension."
  [path]
  (let [dot \.
        filename (get-filename path)
        dot-index (get-last-index dot filename)]
    (if dot-index
      (subs filename dot-index)
      "")))

(defn get-filename-root
  "Given a complete path string (arg1), returns a string that contains just the filename root
 (without extension)."
  [path]
  (let [dot \.
        filename (get-filename path)
        dot-index (get-last-index dot filename)]
    (if dot-index
      (subs filename 0 dot-index)
      filename)))

(defn floor-rem
  "Returns a two-element vector, the first element being the floor of the quotient of n/d,
the second being the remainder of the quotient."
  [n d]
  (let [q (int (/ n d))]
    (vector q (- n (* q d)))))

(defn square
  "Computes the square of the argument."
  [x]
  (* x x))

(defn ave
  "Returns the average value of a numerical sequence. The value 0 is returned if the sequence is empty."
  [& s]
  (if (empty? s)
    0
    (/ (apply + s) (count s))))

;;; Some AI search methods.
(defn tree-search
  "A general function for performing a tree-search, based on the function of the same name
in Peter Norvig's  _Paradigms_of_Artificial_Intelligence_Programming. Unlike Norvig's
implementation, here we use a loop-recur pair to implement the recursion. Consequently,
the input argument states is replaced by initial-state:

initial-state represents the initial node of the search, usually the root node of the
              search tree.
goal?         is a function that returns true if and only if the goal of the search has been
              achieved.
successors    is a function of a single argument that returns a list of successor states of its
              argument.
combiner      is a function of two list arguments new states, followed by old states. It returns
              a new list of states in the order determined by the search strategy."
  [initial-state goal? successors combiner]
  (loop [states (list initial-state)]
    (if (empty? states)
      nil ; Search failed to find a goal.
      (let [next (first states)]
        (if (goal? next)
          next
          (recur (combiner (successors next) (rest states))))))))

(defn depth-first-tree-search
  [start goal? successors]
  (tree-search start goal? successors concat))

(defn breadth-first-tree-search
  [start goal? successors]
  (tree-search start goal? successors #(concat %2 %1)))

#_(defn graph-search
  [initial-state goal? successors combiner]
  (loop [open-nodes (list initial-state) closed-nodes #{}]
    (if (empty? open-nodes)
      nil
      (let [next (first open-nodes)]
        (if (goal? next)
          next
          (let [closed-nodes (conj closed-nodes next)
                children (filter (partial (comp not contains?) closed-nodes)
                                 (successors next))]
            (recur (combiner children (rest open-nodes)) closed-nodes)))))))

;;; In order to test tree search, we need to build a tree:
(defrecord BinaryTreeNode [value left right])

(defn make-binary-tree
  [n]
  (let [tree (BinaryTreeNode. 1 nil nil) level 0]
    (if (= level n)
      tree
      )))
