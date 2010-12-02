;;; test_core.clj
;;;
;;; Tests the utility functions of clj-ctree defined in file
;;; clj_ctree/utils.clj

(ns test_utils
  (:use clj-ctree.utils)
  (:use clojure.test))

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
		 
