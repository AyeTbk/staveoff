(ns staveoff.physics
  (:require [numb.math :refer [rect-overlaps]]
            [staveoff.gameobj :refer [on-collision]]))

(defn is-collider? [obj] (-> obj :tags (contains? :collider)))
(defn is-trigger? [obj] (-> obj :tags (contains? :trigger)))
(defn is-physobj? [obj] (or (is-collider? obj) (is-trigger? obj)))

(defn tick-physics [gameobjs]
  (let [enumerate (fn [coll] (map vector (range) coll))
        physobjs (filter #(is-physobj? (second %)) (enumerate gameobjs))
        colliders (filter #(is-collider? (second %)) (enumerate gameobjs))
        candidate-pairs (for [[i a] physobjs
                              [j b] colliders
                              :when (> j i)]
                          [[i a] [j b]])
        collision-pairs (flatten
                         (for [[[i a] [j b]] candidate-pairs]
                           (if (rect-overlaps a b)
                             (if (is-trigger? a)
                               {:idx i :others [b]}
                               [{:idx i :others [b]} {:idx j :others [a]}])
                             [])))
        collisions-per-obj (into {} (for [idx (set (map :idx collision-pairs))]
                                      (let [all-others-of-idx (flatten
                                                               (for [pair collision-pairs
                                                                     :when (= (:idx pair) idx)]
                                                                 (:others pair)))]
                                        {idx all-others-of-idx})))]
    (for [[i obj] (enumerate gameobjs)]
      (let [collisions (get collisions-per-obj i)]
        (reduce on-collision obj collisions)))))
