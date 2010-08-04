(ns snapp.algebra.vectors) 

(defn inner-product
  "Computes the inner product of two vectors"
  [x y]
  (apply + (map #(* %1 %2) x y)))

(defn vector-add
  "Adds two vectors"
  ([v1] v1)
  ([v1 v2]
  (let [sum (map #(+ %1 %2) v1 v2)
		v1c (count v1)
		v2c (count v2)]
	(if (< v1c v2c)
	  (vec (concat sum (drop v1c v2)))
	  (vec (concat sum (drop v2c v1))))))
  ([v1 v2 & more] (reduce vector-add (vector-add v1 v2) more)))

(defn vector-scale
  "Multiplies a vector v by scalar s"
  [s v]
  (vec (map #(* s %) v)))

(defn basis-vector
 [i n]
 (vec (map #(if (= % i) 1 0) (range n))))
	
