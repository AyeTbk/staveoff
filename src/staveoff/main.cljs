(ns staveoff.main
  (:require [numb.prelude :as numb]
            [staveoff.manager :refer [tick-mgr draw-mgr make-game-manager make-brick-manager]]
            [staveoff.gameobj :refer [cleanup-gameobjs
                                      tick-obj draw-obj]]
            [staveoff.physics :refer [tick-physics]]))


(defonce mgrs [])
(defonce gameobjs [])
(defonce resources {})


(numb/run-game!
 :init
 (fn []
   (set! mgrs [(make-game-manager) (make-brick-manager)])
   (set! gameobjs []))

 :tick
 (fn [input dt]
   (set! mgrs (mapv
               (fn [mgr] (let [[new-mgr new-gameobjs new-resources] (tick-mgr mgr gameobjs resources input dt)]
                           (set! gameobjs new-gameobjs)
                           (set! resources new-resources)
                           new-mgr)) mgrs))
   (set! gameobjs (cleanup-gameobjs (map #(tick-obj % resources input dt) gameobjs)))
   (set! gameobjs (tick-physics gameobjs)))

 :draw
 (fn []
   (flatten
    [:clear
     (for [gameobj gameobjs]
       (draw-obj gameobj))
     (for [mgr mgrs]
       (draw-mgr mgr gameobjs resources))])))


;; TODO
;; Game state management
;; x Start menu
;; x Game begins moment
;; - Gameplay loop
;; - Pause menu?
;; - Win / x loss condition
;; - Victory screen, x Game over screen (tells you *why* you lost)
;; Establish proper play area bounds, design the onslaught with it in mind.
