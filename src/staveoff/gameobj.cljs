(ns staveoff.gameobj
  (:require [staveoff.state :refer [bounds-rect ball-speed descent-animation-duration]]
            [numb.time :refer [make-tween tick-tween]]
            [numb.render :refer [compute-text-rect!]]
            [numb.math :refer [clamp ease-out ease-in-ease-out
                               v+ v- v* vdiv v-dot v-norm v-normalize v-reflect get-x get-y
                               decompose-rect rect-grow-centered rect-contain?]]
            [clojure.math :refer [signum floor]]))


(defn cleanup-gameobjs [gameobjs]
  (vec (filter some? (flatten gameobjs))))


(defmulti tick-obj (fn [obj _resources _input _dt] (:kind obj)))
(defmulti draw-obj (fn [obj] (:kind obj)))
(defmulti on-collision (fn [obj _other] (:kind obj)))

(defmethod tick-obj :default [self _ _ _]
  (throw (js/Error. (str "tick-obj unimplemented for " self))))
(defmethod draw-obj :default [self]
  (throw (js/Error. (str "draw-obj unimplemented for " self))))
(defmethod on-collision :default [self _]
  (throw (js/Error. (str "on-collision unimplemented for " self))))


(defn dir-from-to [from to]
  (v-normalize (v- (-> to decompose-rect :center) (-> from decompose-rect :center))))

(defn dist-from-to [from to]
  (v-norm (v- (-> to decompose-rect :center) (-> from decompose-rect :center))))



;; Paddle

(defn compute-paddle-target-y []
  (-> (:size bounds-rect) get-y (- 40)))

(defn compute-paddle-start-y []
  (-> (:size bounds-rect) get-y))

(defn make-paddle []
  {:kind :paddle
   :pos [0 0]
   :size [75 10]
   :phys-tags #{:collider}
   :spawn-tween (make-tween (compute-paddle-start-y) (compute-paddle-target-y) 0.5 ease-in-ease-out)})

(defmethod tick-obj :paddle
  [self _resources input dt]
  (let [rect (decompose-rect self)
        bounds (decompose-rect bounds-rect)
        half-width (-> rect :width (/ 2))
        mouse-x (-> input :mouse :pos get-x)
        target-center-x (clamp mouse-x (+ (:left bounds) half-width) (- (:right bounds) half-width))
        new-left (- target-center-x half-width)
        [new-spawn-tween new-top _] (tick-tween (:spawn-tween self) dt)]
    (-> self
        (assoc :spawn-tween new-spawn-tween)
        (assoc :pos [new-left new-top]))))

(defmethod draw-obj :paddle
  [self]
  (let [half-size (vdiv (:size self) 2)
        [half-width _] half-size]
    [{:kind :rect :pos (:pos self) :size (:size self) :fill "lightgray" :stroke "darkgray"}
     {:kind :rect :pos (:pos self) :size half-size :fill "red" :stroke "darkgray"}
     {:kind :rect :pos (v+ (:pos self) [half-width 0]) :size half-size :fill "red" :stroke "darkgray"}]))

(defmethod on-collision :paddle
  [self other]
  (if (= (:kind other) :brick)
    nil
    self))



;; Ball

(defn make-ball []
  (let [size 10
        pos-x (-> bounds-rect decompose-rect :center get-x (- (/ size 2)))
        pos-y (- (compute-paddle-target-y) 30)]
    {:kind :ball
     :pos [pos-x pos-y]
     :size [size size]
     :vel (v* [0 -1] ball-speed)
     :phys-tags #{:collider}}))

(defn keep-in-bounds [ball bounds]
  (let [rect (decompose-rect ball)
        bounds (decompose-rect bounds)
        is-outside-left (< (:left rect) (:left bounds))
        is-outside-right (> (:right rect) (:right bounds))
        is-outside-top (< (:top rect) (:top bounds))
        is-outside-bottom (> (:bottom rect) (:bottom bounds))
        new-vel-x (if (or is-outside-left is-outside-right)
                    (- (-> ball :vel get-x))
                    (-> ball :vel get-x))
        new-vel-y (if (or is-outside-top is-outside-bottom)
                    (- (-> ball :vel get-y))
                    (-> ball :vel get-y))
        new-left (cond
                   is-outside-left (:left bounds)
                   is-outside-right (- (:right bounds) (:width rect))
                   :else (:left rect))
        new-top (cond
                  is-outside-top (:top bounds)
                  is-outside-bottom (- (:bottom bounds) (:height rect))
                  :else (:top rect))]
    (-> ball
        (assoc :vel [new-vel-x new-vel-y])
        (assoc :pos [new-left new-top]))))

(defn ball-hit-normal
  "Assumes ball and b overlap. Returns hit normal of ball on b."
  ;; FIXME Potential problem: when there are multiple simultaneous collisions,
  ;; they get resolved one by one, *affecting the ball's velocity* every time,
  ;; which means that all but the first resolution will have the (wrong)
  ;; updated vel instead of the (correct) initial vel.
  ;; I believe this is the reason the ball seems to "cheat" sometimes, passing
  ;; through multiple bricks when it clearly shouldn't.
  ;; Maybe substepping can reduce the amount of simultaneous collision, making
  ;; this problem not so problematic.
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
        reflected-vel-dir (if hit-top-of-paddle
                            hit-normal
                            (v-reflect (v-normalize (:vel ball)) hit-normal))
        reflected-vel (v* reflected-vel-dir ball-speed)
        hit-depth  (ball-hit-depth ball paddle)
        new-pos (v- (:pos ball) hit-depth)
        ball (-> ball (assoc :vel reflected-vel) (assoc :pos new-pos))]
    ball))

(defmethod tick-obj :ball
  [self _resources _input dt]
  (-> self
      (assoc :pos (v+ (:pos self) (v* (:vel self) dt)))
      (keep-in-bounds bounds-rect)))

(defmethod draw-obj :ball
  [self]
  {:kind :rect :pos (:pos self) :size (:size self) :fill "red"})

(defmethod on-collision :ball
  [self other]
  (if (= (:kind other) :paddle)
    (ball-bounce-on-paddle self other)
    (ball-bounce-on-rect self other)))



;; Brick

(defn make-brick [pos]
  {:kind :brick
   :pos pos
   :size [45 20]
   :descent-tween (make-tween (get-y pos) (get-y pos) descent-animation-duration ease-out)
   :phys-tags #{:collider :passive}})

(defmethod tick-obj :brick
  [self resources _input dt]
  (let [[old-x old-y] (:pos self)
        descent-tween (if (:descend-i-command-you! resources)
                        (make-tween old-y (+ old-y 25) descent-animation-duration ease-out)
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


;; UI Button

(def button-font "32px sans-serif")

(defn make-button [pos text tag]
  {:kind :button
   :pos pos
   :text text
   :font button-font
   :tags #{tag}
   :h-padding 50
   :v-padding 20
   :ui-state nil})

(defn tagged-with? [obj tag]
  (contains? (:tags obj) tag))

(defn hovered? [button]
  (= (:ui-state button) :hovered))

(defn down? [button]
  (= (:ui-state button) :down))

(defn clicked? [button]
  (= (:ui-state button) :clicked))

(defn button-clicked? [button-tag gameobjs]
  (some #(and (tagged-with? % button-tag) (clicked? %)) gameobjs))

;; FIXME the logic of this is copy pasted from the logic of draw-obj, DRY plz.
(defn compute-button-bounding-rect [button]
  (let [{pos :pos size :size} (compute-text-rect! (:text button) (:font button) (:pos button))
        {rect-pos :pos rect-size :size} (rect-grow-centered {:pos pos :size size} 50 20)]
    {:pos rect-pos :size rect-size}))

(defmethod tick-obj :button
  [self _resources input _dt]
  (let [rect (compute-button-bounding-rect self)
        is-hovered (rect-contain? rect (-> input :mouse :pos))
        is-down (and is-hovered (-> input :mouse :down (contains? :left)))
        is-clicked (and is-hovered (not is-down) (-> input :mouse :just-released (contains? :left)))]
    (cond
      is-clicked (assoc self :ui-state :clicked)
      is-down (assoc self :ui-state :down)
      is-hovered (assoc self :ui-state :hovered)
      :else (assoc self :ui-state nil))))

(defmethod draw-obj :button
  [self]
  (let [{pos :pos size :size text-pos-offset :text-pos-offset} (compute-text-rect! (:text self) (:font self) (:pos self))
        {rect-pos :pos rect-size :size} (rect-grow-centered {:pos pos :size size} (:h-padding self) (:v-padding self))
        text-pos (v+ pos text-pos-offset)
        color (cond
                (down? self) "#222"
                (hovered? self) "#555"
                :else "#333")]
    [{:kind :rect :pos rect-pos :size rect-size :fill color :stroke "lightgrey"}
     {:kind :text :pos text-pos :text (:text self) :font (:font self) :fill "white"}]))



;; Game UI

(defn make-ui-timer []
  {:kind :ui-timer
   :elapsed 0})

(defmethod tick-obj :ui-timer
  [self resources _input dt]
  (let [should-tick (= (:game-state resources) :game)]
    (cond-> self
      should-tick (update :elapsed + dt))))

(defmethod draw-obj :ui-timer
  [self]
  (let [seconds (floor (rem (:elapsed self) 60))
        minutes (quot (:elapsed self) 60)
        ss (if (< seconds 10)
             (str "0" seconds)
             (str seconds))
        mm (if (< minutes 10)
             (str "0" minutes)
             (str minutes))
        text (str mm ":" ss)]
    [{:kind :text :text text :pos [80 60] :font "36px sans-serif" :line-width 1 :fill "white" :stroke "grey"}]))



(defn make-upgrade-button [pos text tag]
  (-> (make-button pos text tag)
      (assoc :font "16px sans-serif")
      (assoc :h-padding 20)
      (update :tags conj :upgrade-button)))
