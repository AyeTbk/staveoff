(ns staveoff.main
  (:require [numb.prelude :as numb]
            [numb.math :refer [v-]]
            [staveoff.state :refer [mgrs gameobjs resources bounds-rect ui-width]]
            [staveoff.manager :refer [tick-mgr draw-mgr
                                      make-game-manager make-brick-manager make-upgrade-manager]]
            [staveoff.gameobj :refer [cleanup-gameobjs tick-obj draw-obj]]
            [staveoff.physics :refer [tick-physics]]))



(numb/run-game!
 :init
 (fn []
   (set! mgrs [(make-game-manager) (make-brick-manager) (make-upgrade-manager)])
   (set! gameobjs [])
   (set! bounds-rect (let [size (v- (v- (numb/canvas-size) [ui-width 0])
                                    ;; HACK itch is silly and my framework isnt prepared for this
                                    [300 0])
                           pos [ui-width 0]]
                       {:pos pos :size size})))

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
