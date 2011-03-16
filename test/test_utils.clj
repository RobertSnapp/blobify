;;; test_core.clj
;;;
;;; Tests the utility functions of clj-ctree defined in file
;;; clj_ctree/utils.clj

(ns test_utils
  (:use clj-ctree.utils)
  (:use clojure.test))

;;; string opeations
(deftest get-directory-test
  (let [sep (java.io.File/separatorChar)
        dot \.
        p1 (str sep "root" sep "subdir1" sep "subdir2" sep "fileroot" dot "ext")
        p1dir (str sep "root" sep "subdir1" sep "subdir2" sep)
        p1root "fileroot"
        p1ext (str dot "ext")
        p2 (str "fileroot" dot "ext")
        p2dir ""
        p2root "fileroot"
        p2ext (str dot "ext")
        p3 (str sep "topdir" sep "subdirWithDotInName" dot "ok" sep "subdir2" sep "filenamewithoutext")
        p3dir (str sep "topdir" sep "subdirWithDotInName" dot "ok" sep "subdir2" sep)
        p3root "filenamewithoutext"
        p3ext ""]
    (is (= (get-directory p1) p1dir))
    (is (= (get-filename-root p1) p1root))
    (is (= (get-filename-ext p1) p1ext))

    (is (= (get-directory p2) p2dir))
    (is (= (get-filename-root p2) p2root))
    (is (= (get-filename-ext p2) p2ext))

    (is (= (get-directory p3) p3dir))
    (is (= (get-filename-root p3) p3root))
    (is (= (get-filename-ext p3) p3ext))

    ))
(deftest int-to-ubyte-test
  (for [i (range 255)]
	(is (= (ubyte-to-int (int-to-ubyte i)) i))))

(deftest seq2map-test
  (let [times10 #(vector % (* 10 %))
		input-seq '(1 2 3 4 5)
		output-map {1 10, 2 20, 3 30, 4 40, 5 50}]
	(is (= (seq2map input-seq times10) output-map))))

(deftest seq2redundant-map-test
  (let [input-seq [4 2 3 4 4 1 6 3 4],
		ifcn #(vector % %),
		sorted-up-map (seq2redundant-map input-seq ifcn conj :sort-up),
		sorted-down-map (seq2redundant-map input-seq ifcn conj :sort-down),
		unsorted-map (seq2redundant-map input-seq ifcn conj),
		sorted-up-seq '([1 (1)] [2 (2)] [3 (3 3)] [4 (4 4 4 4)] [6 (6)]),
		sorted-down-seq (reverse sorted-up-seq)]
	(is (map? sorted-up-map))
	(is (map? sorted-down-map))
	(is (map? unsorted-map))
	(is (= (seq sorted-up-map) sorted-up-seq))
	(is (= (seq sorted-down-map) sorted-down-seq))
	(is (= (get unsorted-map 1) '(1)))
	(is (= (get unsorted-map 2) '(2)))
	(is (= (get unsorted-map 3) '(3 3)))
	(is (= (get unsorted-map 4) '(4 4 4 4)))
	(is (= (get unsorted-map 6) '(6)))))

(deftest least-above-test
  (is (= (least-above 3 '(1 2 4 5)) 4))
  (is (= (least-above -1 '(1 -2 3/4 -1/2 0)) -1/2 ))
  (is (nil? (least-above 3 '(1 2 3)))))

(deftest greatest-below-test
  (is (= (greatest-below 3 '(1 2 4 5)) 2))
  (is (= (greatest-below -1 '(1 -2 3/4 -1/2 0)) -2 ))
  (is (nil? (greatest-below 0 '(3 2 0 1)))))

(deftest absolutely-within-test
  (is (absolutely-within 0.001 1.0001 1.0002))
  (is (not (absolutely-within 0.001 1.001 2.002))))

(deftest relative-difference-test
  (is (= (relative-difference 2 1) 1)))

(deftest relatively-within-test
  (is (relatively-within 0.01 1.0001 1.0))
  (is (not (relatively-within 0.01 2.001 1.0))))

;;; position tests
(deftest pos-test
  (let [int30 (take 30 (cycle (range 6)))
	    odd-locs (vec (range 1 30 2))
		unit-locs (vec (range 1 30 6))]
	(= (pos-if odd? int30) (first odd-locs))
	(= (pos-if* odd? int30) odd-locs)
	(= (pos-value 1 int30) 1)
	(= (pos-value* 1 int30) unit-locs)
	))

(deftest get-index-list-test
  (let [seq20 '(a b a a b 5 a a b a 10 a a b a 15 a a a b)
		b-locs '(1 4 8 13 19)]
	(= (get-index 'b seq20) 1)
	(nil? (get-index 'c seq20))
	(= (get-index-list 'b seq20) b-locs)
	(= (get-index-list 'c seq20) ())))
		 
;;; Shift registers
(deftest shift-seq-test
  (let [seq3  '(a b c)
		seq3r '(c a b)
		seq3l '(b c a)
		seq1 '(a)]
	(is (= (shift-seq-left  seq3) seq3l))
	(is (= (shift-seq-right seq3) seq3r))
	(is (= (shift-seq-left  seq1) seq1))
	(is (= (shift-seq-right seq1) seq1))
	(is (= (shift-seq-left  ()) ()))
	(is (= (shift-seq-right ()) ()))))


;;; Tests for debug

(deftest debug-test
  (debug :dbg-test1 :dbg-test2)
  (is (true? (contains? (get-debug-ids) :dbg-test1)))
  (is (true? (contains? (get-debug-ids) :dbg-test2)))
  (is (nil? (dbg :dbg-test1 "debug-test: No statements should appear between this line: [line ~a of ~a]~%" 1 3)))
  (undebug :dbg-test1)
  (is (false? (contains? (get-debug-ids) :dbg-test1)))
  (is (true? (contains? (get-debug-ids) :dbg-test2)))
  (dbg :dbg-test1 "debug-test: ERROR: *** this line should not appear ***~%")
  (set-debug-stream nil)
  (is (nil? (get-debug-stream)))
  (println (dbg :dbg-test2 "debug-test: and this line: [line ~a of ~a]" 2 3))
  (set-debug-stream true)
  (is (nil? (dbg-indent :dbg-test2 5 "debug-test: This line should be indented ~r spaces: [line ~a of ~a]~%" (* 5 3) 3 3)))
  (undebug :dbg-test2)
  )
