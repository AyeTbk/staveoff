(ns numb.input
  (:require [numb.math :refer [v+]]))

(defn make-empty-input-state []
  {:mouse {:pos [0 0] :motion [0 0] :down #{} :just-pressed #{} :just-released #{}}})

(defn update-input-state-post-tick [input-state]
  (-> input-state
      (assoc-in [:mouse :motion] [0 0])
      (assoc-in [:mouse :just-pressed] #{})
      (assoc-in [:mouse :just-released] #{})))

(defn add-input [input-state ev]
  (let [update-pos (fn [input-state]
                     (assoc-in input-state [:mouse :pos] (:pos ev)))
        update-motion (fn [input-state]
                        (let [new-motion (v+ (-> input-state :mouse :motion) (:motion ev))]
                          (assoc-in input-state [:mouse :motion] new-motion)))
        update-button (fn [input-state]
                        (case (:state ev)
                          :down (-> input-state
                                    (assoc-in [:mouse :down] (conj (-> input-state :mouse :down) (:button ev)))
                                    (assoc-in [:mouse :just-pressed] (conj (-> input-state :mouse :just-pressed) (:button ev))))
                          :up (-> input-state
                                  (assoc-in [:mouse :just-released] (conj (-> input-state :mouse :just-released) (:button ev)))
                                  (assoc-in [:mouse :down] (disj (-> input-state :mouse :down) (:button ev))))))
        result (case (:kind ev)
                 :mouse (cond-> input-state
                          true (update-pos)
                          (:motion ev) (update-motion)
                          (:button ev) (update-button))
                 input-state)]
    result))
