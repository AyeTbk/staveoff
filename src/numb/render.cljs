(ns numb.render)


(defonce canvas nil)
(defonce canvas-ctx nil)


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
  (let [;; HACK Easy way to clear the screen: 10000x10000 pixel rectangle.
        ;;width (-> canvas-ctx .-canvas .getBoundingClientRect .-width)
        ;;height (-> canvas-ctx .-canvas .getBoundingClientRect .-height)
        width 10000
        height 10000]
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



(defn compute-text-rect! [text font pos]
  (set! (.-font canvas-ctx) font)
  (let [metrics (.measureText canvas-ctx text)
        descent (.-actualBoundingBoxDescent metrics)
        ascent (.-actualBoundingBoxAscent metrics)
        bb-left (.-actualBoundingBoxLeft metrics)
        w (+ bb-left (.-actualBoundingBoxRight metrics))
        h (+ ascent descent)
        extra-padding-bottom 1
        text-pos-offset [bb-left (- ascent extra-padding-bottom)]]
    {:pos pos :size [w h] :text-pos-offset text-pos-offset}))
