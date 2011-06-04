(defproject clj-ctree "0.1"
  :description "A prototype of the component tree datastructure and related tools for analyzing grayscale voxel arrays."
  :dependencies [[fiji-local/ij "1.0"]
                 [fiji-local/LSM_Toolbox "4.0g"]
                 [fiji-local/LSM_Reader "4.0g"]
                 [org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.4.0-SNAPSHOT"]
                     [clojure-source "1.2.0"]]
  :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
  :aot [clj-ctree.bounding-boxes
        clj-ctree.core
		  clj-ctree.image
		  clj-ctree.main
		  clj-ctree.polynomial
		  clj-ctree.utils
        clj-ctree.vectors]
  :main clj-ctree.main)
