(ns staveoff.main
  (:require [numb.prelude :as numb]
            [staveoff.gameobj :refer [cleanup-gameobjs
                                      tick-obj draw-obj
                                      make-ball make-paddle
                                      make-brick make-brick-manager]]
            [staveoff.physics :refer [tick-physics]]))


(defonce gameobjs [])


(numb/run-game!
 :init
 (fn []
   (set! gameobjs [(make-paddle) (make-ball) (make-brick-manager) (make-brick [250 -20])]))

 :tick
 (fn [input dt]
   (set! gameobjs (cleanup-gameobjs (map #(tick-obj % input dt) gameobjs)))
   (set! gameobjs (tick-physics gameobjs)))

 :draw
 (fn []
   (flatten
    [:clear
     (for [gameobj gameobjs]
       (draw-obj gameobj))])))
