(defproject imageAnalysis-componentTree "0.1"
  :description "A prototype of the component tree datastructure and related tools for analyziing grayscale voxel arrays."
  :dependencies [[org.clojure/clojure "1.1.0"]
				 [org.clojure/clojure-contrib "1.1.0"]
				 [leiningen/lein-swank "1.1.0"]]
  :namespaces [snapp.algebra.vectors
			   snapp.algebra.polynomial
			   snapp.componentTree.core
			   snapp.componentTree.main
			   snapp.componentTree.image]
  :main snapp.componentTree.main)
