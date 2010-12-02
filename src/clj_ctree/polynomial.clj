(ns clj-ctree.polynomial)

(defn horner
  "Given an ascending vector (or seq) of coefficients and a scalar x, horner evaluates
   the corresponding polynomial using Horner's rule. For example, (horner [c0 c1 c2] x)
   evaluates to (c2*x + c1)*x + c0 = c0 + c1 * x + c2 * x^2.
   Also (horner [] x) evaluates to 0."
  [coefficients x]
  (let [c-vector (vec coefficients)]
	(if (empty? c-vector)
	  0
	  (loop [c-rest (pop c-vector) value (peek c-vector)]
		(if (empty? c-rest)
		  value
		  (recur (pop c-rest) (+ (* value x) (peek c-rest))))))))

(defn multi-horner
  "Given an ascending vector (or seq) of coefficents [c0 c1 c2 ... c{n-1}],
   and a vector-valued variable of comparable dimension [x1 x2 ... x{n-1}],
   multi-horner evaluates the polynomial
   (...((c{n-1}*x{n-1} + c{n-2})*x{n-2} + c{n-3})*x{n-3} +...+ c1)*x1 + c0
   = c0 + c1*x1 + c2*x2*x1 + ... + c{n-1}*x{n-1}*x{n-2}*...*x2*x1.
   The coefficient vector should have one more element than the variable vector."
  [c-seq x-seq]
  {:pre [(= (count c-seq) (inc (count x-seq)))]}
  (let [c-vec (vec c-seq)
		x-vec (vec x-seq)]
	(loop [c-rest (pop c-vec) x-rest x-vec value (peek c-vec)]
	  (if (empty? c-rest)
		value
		(recur (pop c-rest) (pop x-rest) (+ (* value (peek x-rest)) (peek c-rest)))))))


(defn interpolate
  "Computes the linear interpolation between two scalar values a and b,
   returning the numeric value of
      a * (1 - x) + b * x."
  [a b x]
  (+ (* a (- 1 x)) (* b x)))
