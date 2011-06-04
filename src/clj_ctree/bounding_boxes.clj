;;; Functions that maintain and manipulate bounding box information

(ns clj-ctree.bounding-boxes
  (:use [clj-ctree.vectors :only (vector-add)]
        [clj-ctree.utils :only (square)]
        clojure.test))

;;; A Box represents an n-dimensional bounding box, or Cartesiain volume. The
;;; record contains a list of intervals, each represented as a vector. Thus
;;; the unit cube would be represented by [[0 1] [0 1] [0 1]]

(defrecord BoundingBox [intervals])


(defprotocol BoundingVolume
  (disjoint? [this v2] "Returns true if the volumes (arg 1 and arg 2) are disjoint.")
  (enlarge-volume [this x] "Returns the smallest volume that includes the original volume (arg1) and vector x (arg2).")
  (get-center [this] "Returns the vector that lies at the center of the bounding volume.")
  (get-dimensionality [this] "Returns the dimensionality of the bounding volume.")
  (get-distance [this v2] "Returns the Euclidean distance between the volumes, arg1 and arg2.")
  (get-distance-to-vector [this x] "Returns the Euclidean distance between the volume (arg1) and a vector (arg2).")
  (get-eccentricity [this] "Returns a measure of the eccentricity of the Bounding volume that ranges from 0 (equliateral or spherical) to 1 (singularly elongated).")
  (get-inner-radius [this] "Returns the radius of largest sphere contained by the volume.")
  (get-outer-radius [this] "Returns the radius of the smallest sphere that contains the volume.")
  (get-volume [this] "Returns the volume of the indicated bounding volume.")
  (inside? [this x] "Returns true if and only if vector x (arg2) is enclosed by the volume (arg1).")
  (merge-volumes [this v2] "Return the smallest volume that includes both volumes.")
  (well-formed? [this] "Returns true if and only if the data structre represents a legitimate volume.")
  )

(defn disjoint-intervals?
  "Returns true if an only if the two (closed) intevals (each implmented as a 2-dimensional vector)
are disjoint, that is they share no point in common."
  [[a b] [c d]]
  {:pre [(<= a b), (<= c d)]}
  (or (< b c) (< d a)))

(deftest disjoint-interval-test
  (is (disjoint-intervals? [0 1] [2 3]))
  (is (disjoint-intervals? [-2.0 -1.0] [0 1]))
  (is (not (disjoint-intervals? [-1 1] [0 2])))
  (is (not (disjoint-intervals? [1/2 1/2] [1/2 1/2]))))

(defn interval-separation
  [[a b] [c d]]
  {:pre [(<= a b) (<= c d)]}
  (cond (<= b c) (- c b)
        (<= c d) (- d c)
        :else 0))

(deftest interval-separation-test
  (is (= 12 (interval-separation [0 2] [14 16])))
  (is (zero? (interval-separation [0 2] [1 3]))))
  
(defn get-bounding-box-sides
  [intervals]
  (map #(- (% 1) (% 0)) intervals))

(extend-type BoundingBox
  BoundingVolume
  (disjoint? [{r1 :intervals} {r2 :intervals}]
    (some true? (map #(disjoint-intervals? %1 %2) r1 r2)))
  (enlarge-volume [{:keys [intervals]} x]
    (BoundingBox. (vec (map #(vector (min (%1 0) %2) (max (%1 1) %2)) intervals x))))
  (get-center [{:keys [intervals]}]
    (vec (map #(/ (apply + %) 2) intervals)))
  (get-dimensionality [{:keys [intervals]}]
    (count intervals))
  (get-distance [{r1 :intervals} {r2 :intervals}]
    (Math/sqrt (apply + (map #(square (interval-separation %1 %2)) r1 r2))))
  (get-distance-to-vector [{:keys [intervals]} x]
    (Math/sqrt (apply + (map #(square (max 0 (- (%1 0) %2) (- %2 (%1 1))) intervals x)))))
  (get-eccentricity [{:keys [intervals]}]
    (let [max-side (apply max (get-bounding-box-sides intervals))
          min-side (apply min (get-bounding-box-sides intervals))]
      (Math/sqrt (- 1  (square (/ min-side max-side))))))
  (get-inner-radius [{:keys [intervals]}]
    (/ (min (get-bounding-box-sides intervals)) 2))
  (get-outer-radius [{:keys [intervals]}]
    (/ (Math/sqrt (apply + (map square (get-bounding-box-sides intervals)))) 2))
  (get-volume [{:keys [intervals]}]
    (apply * (map #(- (% 1) (% 0)) intervals)))
  (inside? [{:keys [intervals]} x]
    (every? true? (map #(<= (%1 0) %2 (%1 1)) intervals x)))
  (merge-volumes [{r1 :intervals} {r2 :intervals} ]
    (BoundingBox. (vec (map #(vector (min (%1 0) (%2 0)) (max (%1 1) (%2 1)))))))
  (well-formed? [{:keys [intervals]}]
    (and (pos? (count intervals))
         (every? #(and (vector? %) (= (count %) 2) (<= (% 0) (% 1))) intervals)))
  )

(defn make-bounding-box
  "Constructs a bounding box using the indicated intervals"
  [intervals]
  (BoundingBox. intervals))

(defn make-bounding-box-at-vector
  "In the first form, constructs a BoundingBox with zero volume about vector v. In the second form,
   a BoundingBox with each side equal to 2*epsilon is centered about v."
  ([v]
     {:pre [(pos? (count v))]}
     (make-bounding-box (vec (map #(vector % %) v))))
  ([v epsilon]
     {:pre [(pos? (count v)), (pos? epsilon)]}
     (make-bounding-box (vec (map #(vector (- % epsilon) (+ % epsilon)))))))

(deftest bounding-box-test
  (def a (make-bounding-box [[0 1] [0 1] [0 1]]))
  (def b (make-bounding-box [[0 1] [0 1] [2 3]]))
  (def c (make-bounding-box [[0 1] [2 3] [0 1]]))
  (def d (make-bounding-box [[2 3] [0 1] [0 1]]))
  (def ab (merge-volumes a b))
  (def ac (merge-volumes a c))
  (def cd (merge-volumes c d))
  (def abcd (merge-volumes ab cd))
  (is (every? well-formed? '(a b c d ab ac cd abcd)))
  (is (disjoint? a b))
  (is (disjoint? a c))
  (is (disjoint? a d))
  (is (disjoint? d (merge-volumes a b)))
  (is (every? #(not (disjoint? a %)) '(a ab abcd)))
  (is (= (get-distance a b) 1))
  (is (= (get-dimensionality abcd) 3))
  (is (every? #(inside? % (get-center %)) '(a b c d ab ac cd abcd))))

