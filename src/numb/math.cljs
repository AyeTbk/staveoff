(ns numb.math
  (:require [clojure.math :refer [sqrt]]))


(defn clamp [val min-val max-val]
  (-> val (min max-val) (max min-val)))


;; Vectors

(defn v-elementwise-binop [op v1 v2]
  (if (number? v2)
    (vec (for [el v1] (op el v2)))
    (vec (map op v1 v2))))

(defn get-x [v] (get v 0))
(defn get-y [v] (get v 1))

(defn v+ [v1 v2] (v-elementwise-binop + v1 v2))
(defn v-
  ([v] (vec (map - v)))
  ([v1 v2] (v-elementwise-binop - v1 v2)))
(defn v* [v1 v2] (v-elementwise-binop * v1 v2))
(defn vdiv [v1 v2] (v-elementwise-binop / v1 v2))
(defn v-dot [v1 v2] (apply + (v* v1 v2)))
(defn v-reflect [v normal] (v- v (v* normal (* 2 (v-dot v normal)))))

(defn v-norm [v]
  (sqrt (v-dot v v)))

(defn v-normalize [v]
  (let [norm (v-norm v)]
    (if (not= norm 0)
      (vdiv v norm)
      [0 0])))


;; Rects

(defn make-rect [& {:keys [pos size]}]
  (let [[width height] size
        [left top] pos
        [right bottom] (v+ pos size)
        center (v+ pos (v* size 0.5))]
    {:top top :left left :bottom bottom :right right :center center :width width :height height}))

(defn rect-overlaps [a b]
  (let [a (make-rect a)
        b (make-rect b)
        overlaps-x (and (<= (:left a) (:right b)) (>= (:right a) (:left b)))
        overlaps-y (and (<= (:top a) (:bottom b)) (>= (:bottom a) (:top b)))]
    (and overlaps-x overlaps-y)))
