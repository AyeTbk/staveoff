(ns numb.render)


(defn render-rect! [canvas-ctx rect fill? stroke?]
  (let [{[x y] :pos [w h] :size} rect]
    (when fill?
      (.fillRect canvas-ctx x y w h))
    (when stroke?
      (.strokeRect canvas-ctx x y w h))))

(defn render-text! [canvas-ctx text fill? stroke?]
  (let [{[x y] :pos text :text font :font} text]
    (when font
      (set! (.-font canvas-ctx) (or font "32px sans-serif")))
    (when fill?
      (.fillText canvas-ctx text x y))
    (when stroke?
      (.strokeText canvas-ctx text x y))))

(defn clear! [canvas-ctx]
  (let [width (-> canvas-ctx .-canvas .getBoundingClientRect .-width)
        height (-> canvas-ctx .-canvas .getBoundingClientRect .-height)]
    (.clearRect canvas-ctx 0 0 width height)))


(defn render-draw-command! [canvas-ctx cmd]
  (let [brush-fill (or (:fill cmd) "gray")
        brush-stroke (or (:stroke cmd) nil)
        brush-width (or (:line-width cmd) 2)
        fill? (not= nil (:fill cmd))
        stroke? (not= nil  brush-stroke)]
    (when (= cmd :clear)
      (clear! canvas-ctx))
    (when fill?
      (set! (.-fillStyle canvas-ctx) brush-fill))
    (when stroke?
      (set! (.-strokeStyle canvas-ctx) brush-stroke)
      (set! (.-lineWidth canvas-ctx) brush-width))
    (case (:kind cmd)
      :rect (render-rect! canvas-ctx cmd fill? stroke?)
      :text (render-text! canvas-ctx cmd fill? stroke?)
      nil)))
