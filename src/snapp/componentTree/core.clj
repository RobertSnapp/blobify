(ns snapp.componentTree.core
  (:use snapp.componentTree.image))

(defn seq2map
  "Constructs a map from a sequence by applying keyvalfn to each
   element of the sequence.  keyvalfn should return a pair [key val]
   to be added to the map for each input sequence element."
  [aseq keyvalfn]
  (into {} (map keyvalfn aseq)))

(defn seq2redundant-map
  "Constructs a map from a sequence by applying keyvalfn to each
   element of the sequence.  keyvalfn should return a pair [key val]
   to be added to the map for each input sequence element.  If key is
   already in the map, its current value will be combined with the new
   val using (mergefn curval val)."
 ([aseq keyvalfn mergefn]
  (reduce (fn [amap x]
            (let [[key val] (keyvalfn x)]
              (update-in amap [key] mergefn val)))
		  {}
          aseq))
 ([aseq keyvalfn mergefn sorted?]
	(reduce (fn [amap x]
            (let [[key val] (keyvalfn x)]
              (update-in amap [key] mergefn val)))
		  (sorted-map)
          aseq)))

(defn bin-by-intensity
  [{:keys [dimensions raster] :as image}]
  (seq2redundant-map (range (apply * dimensions)) #(vector (raster %) %) conj :sorted))

(def test-image [34 45 65 12 3 5 12 3 3 34])

(def ramp-image (make-image-from-fn [32 32] intensity-ramp))

(bin-by-intensity ramp-image)

;; (bin-by-intensity {:raster test-image :rows 1 :cols 10})
