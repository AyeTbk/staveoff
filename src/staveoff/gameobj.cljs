(ns staveoff.gameobj
  (:require [numb.prelude :as numb]
            [numb.time :refer [make-timer tick-timer
                               make-tween tick-tween]]
            [numb.math :refer [clamp
                               v+ v- v* vdiv v-dot v-norm v-normalize v-reflect get-x get-y
                               decompose-rect]]
            [clojure.math :refer [signum]]))


(defn cleanup-gameobjs [gameobjs]
  (vec (filter some? (flatten gameobjs))))


(defmulti tick-obj (fn [obj _input _dt] (:kind obj)))
(defmulti draw-obj (fn [obj] (:kind obj)))
(defmulti on-collision (fn [obj _other] (:kind obj)))

(defmethod tick-obj :default [self _ _]
  (throw (js/Error. (str "tick-obj unimplemented for " self))))
(defmethod draw-obj :default [self]
  (throw (js/Error. (str "draw-obj unimplemented for " self))))
(defmethod on-collision :default [self _]
  (throw (js/Error. (str "on-collision unimplemented for " self))))


(defn dir-from-to [from to]
  (v-normalize (v- (-> to decompose-rect :center) (-> from decompose-rect :center))))

(defn dist-from-to [from to]
  (v-norm (v- (-> to decompose-rect :center) (-> from decompose-rect :center))))



;; Ball

(defn make-ball []
  {:kind :ball
   :pos [0 0]
   :size [10 10]
   :vel [0 300]
   :phys-tags #{:collider}})

(defn keep-in-bounds [ball [width height]]
  (let [rect (decompose-rect ball)
        is-outside-left (< (:left rect) 0)
        is-outside-right (> (:right rect) width)
        is-outside-top (< (:top rect) 0)
        is-outside-bottom (> (:bottom rect) height)
        new-vel-x (if (or is-outside-left is-outside-right)
                    (- (-> ball :vel get-x))
                    (-> ball :vel get-x))
        new-vel-y (if (or is-outside-top is-outside-bottom)
                    (- (-> ball :vel get-y))
                    (-> ball :vel get-y))
        new-left (cond
                   is-outside-left 0
                   is-outside-right (- width (:width rect))
                   :else (:left rect))
        new-top (cond
                  is-outside-top 0
                  is-outside-bottom (- height (:height rect))
                  :else (:top rect))]
    (-> ball
        (assoc :vel [new-vel-x new-vel-y])
        (assoc :pos [new-left new-top]))))

(defn ball-hit-normal
  "Assumes ball and b overlap. Returns hit normal of ball on b."
  [ball b]
  (let [ball-to-b (dir-from-to ball b)
        ball-rect (decompose-rect ball)
        b-rect (decompose-rect b)
        depth-x (if (>= (-> ball :vel get-x) 0)
                  (- (:right ball-rect) (:left b-rect))
                  (- (:left ball-rect) (:right b-rect)))
        depth-y (if (>= (-> ball :vel get-y) 0)
                  (- (:bottom ball-rect) (:top b-rect))
                  (- (:top ball-rect) (:bottom b-rect)))
        hit-normal (if (<= (abs depth-x) (abs depth-y))
                     [(- (signum (get-x ball-to-b))) 0]
                     [0 (- (signum (get-y ball-to-b)))])
        vel-corrected-hit-normal (if (> (v-dot hit-normal (:vel ball)) 0)
                                   (v- hit-normal)
                                   hit-normal)]
    vel-corrected-hit-normal))

(defn ball-hit-depth
  "Assumes ball and b overlap. Returns hit depth of ball and b."
  [ball b]
  (let [ball-to-b (dir-from-to ball b)
        ball-rect (decompose-rect ball)
        b-rect (decompose-rect b)
        depth-x (if (>= (get-x ball-to-b) 0)
                  (- (:right ball-rect) (:left b-rect))
                  (- (:left ball-rect) (:right b-rect)))
        depth-y (if (>= (get-y ball-to-b) 0)
                  (- (:bottom ball-rect) (:top b-rect))
                  (- (:top ball-rect) (:bottom b-rect)))]
    (if (<= (abs depth-x) (abs depth-y))
      [depth-x 0]
      [0 depth-y])))

(defn ball-bounce-on-rect [ball other]
  (let [hit-normal (ball-hit-normal ball other)
        reflected-vel (v-reflect (:vel ball) hit-normal)
        hit-depth  (ball-hit-depth ball other)
        new-pos (v- (:pos ball) hit-depth)
        ball (-> ball (assoc :vel reflected-vel) (assoc :pos new-pos))]
    ball))

(defonce ball-paddle-bounce-accel 1.015)

(defn ball-bounce-on-paddle [ball paddle]
  (let [true-hit-normal (ball-hit-normal ball paddle)
        paddle-to-ball-dir (dir-from-to paddle ball)
        control-factor (let [max-dist 15
                             dist (dist-from-to paddle ball)]
                         (- 1 (/ (clamp (- max-dist dist) 0 max-dist) max-dist)))
        hit-top-of-paddle (> (v-dot true-hit-normal [0 -1]) 0)
        hit-normal (if hit-top-of-paddle
                     (v-normalize (v+ true-hit-normal (v* paddle-to-ball-dir control-factor)))
                     true-hit-normal)
        reflected-vel (if hit-top-of-paddle
                        (v* hit-normal (-> ball :vel v-norm))
                        (v-reflect (:vel ball) hit-normal))
        accelerated-vel (v* reflected-vel ball-paddle-bounce-accel)
        hit-depth  (ball-hit-depth ball paddle)
        new-pos (v- (:pos ball) hit-depth)
        ball (-> ball (assoc :vel accelerated-vel) (assoc :pos new-pos))]
    ball))

(defmethod tick-obj :ball
  [self _input dt]
  (-> self
      (assoc :pos (v+ (:pos self) (v* (:vel self) dt)))
      (keep-in-bounds (numb/canvas-size))))

(defmethod draw-obj :ball
  [self]
  {:kind :rect :pos (:pos self) :size (:size self) :fill "red"})

(defmethod on-collision :ball
  [self other]
  (if (= (:kind other) :paddle)
    (ball-bounce-on-paddle self other)
    (ball-bounce-on-rect self other)))



;; Paddle

(defn make-paddle []
  {:kind :paddle
   :pos [0 (-> (numb/canvas-size) get-y (- 40))]
   :size [75 10]
   :phys-tags #{:collider}})

(defmethod tick-obj :paddle
  [self input _dt]
  (let [rect (decompose-rect self)
        half-width (-> rect :width (/ 2))
        mouse-x (-> input :mouse :pos get-x)
        target-center-x (clamp mouse-x half-width (-> (numb/canvas-size) get-x (- half-width)))
        new-left (- target-center-x half-width)
        new-top (:top rect)]
    (assoc self :pos [new-left new-top])))

(defmethod draw-obj :paddle
  [self]
  (let [half-size (vdiv (:size self) 2)
        [half-width _] half-size]
    [{:kind :rect :pos (:pos self) :size (:size self) :fill "lightgray" :stroke "darkgray"}
     {:kind :rect :pos (:pos self) :size half-size :fill "red" :stroke "darkgray"}
     {:kind :rect :pos (v+ (:pos self) [half-width 0]) :size half-size :fill "red" :stroke "darkgray"}]))

(defmethod on-collision :paddle
  [self _]
  self)



;; Brick manager

(defonce descent-delay 3)
(defonce descend-i-command-you! false)

(declare make-brick)
(defn make-brick-manager []
  {:kind :brick-manager
   :descent-timer (make-timer 0 :cyclic)})

(defmethod tick-obj :brick-manager
  [self _input dt]
  (let [[descent-timer descent-triggered] (tick-timer (:descent-timer self) dt)
        new-descent-timer (if descent-triggered
                            (-> descent-timer
                                (assoc :duration descent-delay)
                                (assoc :elapsed 0))
                            descent-timer)
        new-self (assoc self :descent-timer new-descent-timer)]
    (set! descend-i-command-you! false)
    (when descent-triggered
      (set! descend-i-command-you! true))
    [new-self
     (if descent-triggered
       (vec (for [x (range 10)]
              (make-brick [(+ (* x (+ 45 5)) 120) -25])))
       [])]))

(defmethod draw-obj :brick-manager
  [_]
  [])



;; Brick

(defn make-brick [pos]
  {:kind :brick
   :pos pos
   :size [45 20]
   :descent-tween (make-tween (get-y pos) (get-y pos) 0.25)
   :phys-tags #{:collider :passive}})

(defmethod tick-obj :brick
  [self _input dt]
  (let [[old-x old-y] (:pos self)
        descent-tween (if descend-i-command-you!
                        (make-tween old-y (+ old-y 25) 0.25)
                        (:descent-tween self))
        [descent-tween tweened-y _finished] (tick-tween descent-tween dt)
        new-pos [old-x tweened-y]]
    (-> self
        (assoc :descent-tween descent-tween)
        (assoc :pos new-pos))))

(defmethod draw-obj :brick
  [self]
  {:kind :rect :pos (:pos self) :size (:size self) :fill "green" :stroke "darkgreen"})

(defmethod on-collision :brick
  [_ _]
  nil)
