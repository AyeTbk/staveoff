(ns staveoff.main
  (:require [numb.prelude :as numb]
            [staveoff.gameobj :refer [cleanup-gameobjs
                                      tick-obj draw-obj
                                      make-ball make-paddle make-brick-manager]]
            [staveoff.physics :refer [tick-physics]]))


(defonce gameobjs [])


(numb/run-game!
 :init
 (fn []
   (set! gameobjs [(make-brick-manager) (make-paddle) (make-ball)]))

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


;; TODO
;; Game state management
;; - Start menu
;; - Game begins moment
;; - Gameplay loop
;; - Pause menu?
;; - Win/loss condition
;; - Victory screen, Game over screen
;; Establish proper play area bounds, design the onslaught with it in mind.
