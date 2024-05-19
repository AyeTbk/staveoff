(ns staveoff.physics
  (:require [numb.math :refer [rect-overlaps]]
            [staveoff.gameobj :refer [cleanup-gameobjs on-collision]]))

(defn is-collider? [obj] (-> obj :phys-tags (contains? :collider)))
(defn is-trigger? [obj] (-> obj :phys-tags (contains? :trigger)))
(defn is-passive? [obj] (-> obj :phys-tags (contains? :passive)))
(defn is-physobj? [obj] (or (is-collider? obj) (is-trigger? obj)))

(defn tick-physics [gameobjs]
  (let [enumerate (fn [coll] (map vector (range) coll))
        physobjs (filter #(let [obj (second %)]
                            (and (is-physobj? obj) (not (is-passive? obj)))) (enumerate gameobjs))
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
                                        {idx all-others-of-idx})))
        resolved-gameobjs (cleanup-gameobjs
                           (for [[i obj] (enumerate gameobjs)]
                             (let [collisions (get collisions-per-obj i)]
                               (reduce
                                #(cond
                                   (nil? %1) (reduced nil)
                                   (vector? %1) (reduced %1)
                                   :else (on-collision %1 %2)) obj collisions))))]
    resolved-gameobjs))
