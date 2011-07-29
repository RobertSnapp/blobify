;;; graphics.clj is part of blobify.
;;;
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

;;; Graphical routines and utilities

(ns blobify.graphics
  (:import (java.awt.image BufferedImage)
           (java.awt Color Graphics Dimension)
           (javax.swing JPanel JFrame JLabel))
  (:use [clojure.contrib.math :only (abs)]))

(defn hsv-to-rgb
  "Returns a vector that contains the red, green and blue color components for the color
specified by its hue, saturation and value."
  [h s v]
  (let [c (* v s)                       ; chroma
        hp (/ (mod h 360) 60)           ; hp is between 0 and 6
        x (* c (- 1 (abs (- (mod hp 2) 1))))
        m (- v c)
        [r g b] (case (int hp)
                      0 [c x 0]
                      1 [x c 0]
                      2 [0 c x]
                      3 [0 x c]
                      4 [x 0 c]
                      5 [c 0 x]
                      [0 0 0])]
    (vec (map #(int (* 255 (+ % m))) [r g b]))))

(defn get-indexed-rgb
  "Generates a random color map that can be used for labeling components. The argument k should
be a non-negative integer."
  [k]
  (let [n (+ k 1)
        h (* 360 (mod (* n (Math/sqrt 0.014)) 1))
        r (mod (* n (Math/sqrt 1.5)) 1)
        s (+ 0.5 (* 0.5 r))
        t (mod (* n (Math/sqrt 5.0)) 1)
        v (+ 0.5 (* 0.5 t))]
    (hsv-to-rgb h s v)))

(def max-height 1024)
(def max-width 1600)
(defn render-indexed-rgb
  "Display the leading indexed colors in a rectangular tiled array."
  [cols rows]
  (let [frame (JFrame. "Indexed Colors (RGB)")
        ; scale (min (/ max-side (max cols rows)) 40)
        scale (min (/ max-height rows) (/ max-width cols) 40)
        buffer (new BufferedImage
                    (* scale cols)
                    (* scale rows)
                    BufferedImage/TYPE_INT_RGB)
        canvas (proxy [JLabel] []
                 (paint [g] (.drawImage g buffer 0 0 this)))
        graphics (.createGraphics buffer)]
    (doseq [x (range cols)
            y (range rows)]
      (let [n (+ x (* cols y))
            [r g b] (get-indexed-rgb n)]
        (doto graphics
          (.setColor (new Color r g b))
          (.fillRect (* x scale) (* y scale) scale scale))))
    
    (.add frame canvas)
    (.setSize frame (new Dimension (* scale cols) (+ 20 (* scale rows))))
    (.show frame))
  )
