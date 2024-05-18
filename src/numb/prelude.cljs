(ns numb.prelude
  (:require [numb.internal]))

(defn canvas-size [] [(numb.internal/get-canvas-width) (numb.internal/get-canvas-height)])

(defn run-game! [& {:keys [init tick draw]}]
  (numb.internal/run-game! :init init :tick tick :draw draw))
