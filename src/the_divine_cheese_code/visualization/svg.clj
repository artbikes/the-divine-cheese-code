(ns the-divine-cheese-code.visualization.svg
  (:require [clojure.string :as s])
  (:refer-clojure :exclude [min max]))

(defn comprator-over-maps
  [comparison-fn ks]
  (fn [maps]
    (zipmap ks
            (map (fn [k] (apply comparison-fn (map k maps)))
                 ks))))

(def min (comprator-over-maps clojure.core/min [:lat :lng]))
(def max (comprator-over-maps clojure.core/max [:lat :lng]))

;; (min [{:a 1 :b 3} {:a 5 :b 0}])
;; (zipmap [:a :b] [1 2])
;; (merge-with - {:lat 50 :lng 10} {:lat 5 :lng 3})
;; 
(defn translate-to-00
  [locations]
  (let [mincoords (min locations)]
    (map #(merge-with - % mincoords) locations)))

(defn scale
  [width height locations]
  (let [maxcoords (max locations)
        ratio {:lat (/ height (:lat maxcoords))
               :lng (/ width (:lng maxcoords))}]
    (map #(merge-with * % ratio) locations)))

(defn latlng->point
  "Convert lat/lng map to comma separated string"
  [latlng]
  (str (:lat latlng) "," (:lng latlng)))

(defn points
  [locations]
  (clojure.string/join " " (map latlng->point locations)))

(defn line
  [points]
  (str "<polyline points=\"" points "\" />"))

(defn transform
  "Just chains other functions"
  [width height locations]
  (->> locations
       translate-to-00
       (scale width height)))

(defn xml
  "svg 'template', which also flips the coordinate system"
  [width height locations]
  (str "<svg height=\"" height "\" width=\"" width "\">"
       ;; These two <g> tags change the coordinate system so that
       ;; 0,0 is in the lower-left corner, instead of SVG's default
       ;; upper-left corner
       "<g transform=\"translate(0," height ")\">"
       "<g transform=\"rotate(-90)\">"
       (-> (transform width height locations)
           points
           line)
       "</g></g>"
       "</svg>"))

