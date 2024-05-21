(ns numb.math
  (:require [clojure.math :refer [sqrt pow cos PI]]))


(defn clamp [val min-val max-val]
  (-> val (min max-val) (max min-val)))


;; Interpolation

(defn lerp [from to t]
  (+ from (* (- to from) t)))

(defn ease-in-ease-out [from to t]
  (let [eased-t (/ (+ (cos (* PI (+ 1 t))) 1) 2)]
    (lerp from to eased-t)))

(defn ease-out [from to t]
  (let [eased-t (- 1 (pow (- t 1) 2))]
    (lerp from to eased-t)))


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

(defn decompose-rect [& {:keys [pos size]}]
  (let [[width height] size
        [left top] pos
        [right bottom] (v+ pos size)
        center (v+ pos (v* size 0.5))]
    {:top top :left left :bottom bottom :right right :center center :width width :height height}))

(defn rect-overlaps? [a b]
  (let [a (decompose-rect a)
        b (decompose-rect b)
        overlaps-x (and (<= (:left a) (:right b)) (>= (:right a) (:left b)))
        overlaps-y (and (<= (:top a) (:bottom b)) (>= (:bottom a) (:top b)))]
    (and overlaps-x overlaps-y)))

(defn rect-contain? [a [x y]]
  (let [a (decompose-rect a)
        contains-x (and (<= (:left a) x) (>= (:right a) x))
        contains-y (and (<= (:top a) y) (>= (:bottom a) y))]
    (and contains-x contains-y)))

(defn rect-grow-centered [r horizontal vertical]
  (let [decomposed (decompose-rect r)
        size [(+ (:width decomposed) horizontal) (+ (:height decomposed) vertical)]
        offset (v* size 0.5)
        pos (v- (:center decomposed) offset)]
    (-> r
        (assoc :pos pos)
        (assoc :size size))))
