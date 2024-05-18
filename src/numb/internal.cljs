(ns numb.internal
  (:require [numb.input]
            [numb.render]))


(def tick-callback (fn [_input _dt]))
(def draw-callback (fn []))

(def input (numb.input/make-empty-input-state))
(def input-callback (fn [ev]
                      (set! input (numb.input/add-input input ev))))

(def canvas nil)
(def canvas-ctx nil)

(defn get-canvas-width [] (.-width (.getBoundingClientRect canvas)))
(defn get-canvas-height [] (.-height (.getBoundingClientRect canvas)))

(defn resize-callback []
  (.setAttribute canvas "width" (get-canvas-width))
  (.setAttribute canvas "height" (get-canvas-height)))


(def mousemove-previous-pos nil)
(def mousemove-motion nil)
(defn register-event-listeners! []
  (let [mouse-pos (fn [ev] [(.-offsetX ev) (.-offsetY ev)])
        mousemove-pos (fn [ev]
                        (let [pos (mouse-pos ev)]
                          (when (not= mousemove-previous-pos nil)
                            (set! mousemove-motion (vec (map - pos mousemove-previous-pos))))
                          (set! mousemove-previous-pos pos)
                          pos))]

  ;; Resize
    (.addEventListener js/window "resize" resize-callback)

  ;; Input
    (.addEventListener canvas "keydown"
                       (fn [ev]
                         (input-callback {:kind :kb, :state :down, :key (.-code ev), :text (.-key ev)})))
    (.addEventListener canvas "keyup"
                       (fn [ev]
                         (input-callback {:kind :kb, :state :up, :key (.-code ev), :text (.-key ev)})))

    (.addEventListener canvas "mousedown"
                       (fn [ev]
                         (input-callback {:kind :mouse,
                                          :pos (mouse-pos ev),
                                          :state :down
                                          :button (case (.-button ev)
                                                    0 :left
                                                    1 :middle
                                                    2 :right)})))
    (.addEventListener canvas "mouseup"
                       (fn [ev]
                         (input-callback {:kind :mouse,
                                          :pos (mouse-pos ev),
                                          :state :up
                                          :button (case (.-button ev)
                                                    0 :left
                                                    1 :middle
                                                    2 :right)})))
    (.addEventListener canvas "mousemove"
                       (fn [ev]
                         (let [pos (mousemove-pos ev)]
                           (when (not= mousemove-motion nil)
                             (input-callback {:kind :mouse,
                                              :pos pos,
                                              :motion mousemove-motion})))))
    (.addEventListener canvas "contextmenu"
                       (fn [ev]
                         (.preventDefault ev)))))


(defn start-update-loop! []
  (letfn [(update-loop! [previous-timestamp]
            (.requestAnimationFrame
             js/window
             (fn [timestamp]
               (when (not= previous-timestamp 0)
                 (let [delta-time (/ (- timestamp previous-timestamp) 1000)]
                   (tick-callback input delta-time)
                   (set! input (numb.input/update-input-state-post-tick input))
                   (let [draw-cmds (draw-callback)]
                     (doseq [cmd draw-cmds]
                       (numb.render/render-draw-command! canvas-ctx cmd)))))
               (update-loop! timestamp))))]
    (update-loop! 0)))


(defn initialize! []
  (set! canvas (.getElementById js/document "canvas"))
  (set! canvas-ctx (.getContext canvas "2d"))
  (register-event-listeners!)
  (start-update-loop!))


(defonce initialized false)
(defn run-game! [& {:keys [init tick draw]}]
  (set! tick-callback tick)
  (set! draw-callback draw)
  (when (not initialized)
    (set! initialized true)
    (initialize!)
    (init)))
