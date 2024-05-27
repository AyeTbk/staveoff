(ns staveoff.gameobj
  (:require [staveoff.state :refer [bounds-rect ball-speed descent-animation-duration game-time
                                    pfx-paddle-destroyed pfx-brick-destroyed pfx-brick-attack
                                    money]]
            [numb.time :refer [make-tween tick-tween]]
            [numb.render :refer [compute-text-rect!]]
            [numb.math :refer [clamp ease-out ease-in-ease-out
                               v+ v- v* vdiv v-dot v-norm v-normalize v-reflect get-x get-y
                               decompose-rect rect-grow-centered rect-scale rect-contain?]]
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



;; Particle

(defn make-particle [pos desc]
  (let [life-span ((:life-span desc))
        size ((:start-size desc))
        vel ((:start-vel desc))
        emits (vec
               (flatten
                (for [emit-desc (:emits desc)
                      :when (some? (:desc emit-desc))]
                  (let [desc (:desc emit-desc)
                        count (floor ((:count emit-desc)))
                        interval (/ (:span emit-desc) count)
                        start-time (- (:time emit-desc) (/ (:span emit-desc) 2))]
                    (for [i (range count)]
                      {:desc desc
                       :emit-at-time (+ start-time (* i interval))})))))]
    {:kind :particle
     :pos pos
     :size size
     :life-span life-span
     :elapsed 0
     :vel vel
     :fill-fn (:fill desc)
     :stroke-fn (:stroke desc)
     :line-width-fn (:line-width desc)
     :scale-fn (:scale desc)
     :gravity-fn (:gravity desc)
     :emits emits}))

(defn compute-particle-t [particle]
  (clamp (/ (:elapsed particle) (:life-span particle)) 0 1))

(defmethod tick-obj :particle
  [self _resources _input dt]
  (let [ticked-self (update self :elapsed + dt)
        should-be-destroyed (> (:elapsed ticked-self) (:life-span ticked-self))
        t (compute-particle-t ticked-self)
        gravity ((:gravity-fn ticked-self) t)
        acceled-self (update ticked-self :vel v+ (v* gravity dt))
        moved-self (update acceled-self :pos v+ (v* (:vel acceled-self) dt))
        [emitted new-emits] (letfn [(ready-to-be-emitted? [emit] (<= (:emit-at-time emit) t))]
                              [(take-while ready-to-be-emitted? (:emits moved-self))
                               (vec (drop-while ready-to-be-emitted? (:emits moved-self)))])
        new-particles (vec (map #(make-particle (:pos moved-self) (:desc %)) emitted))
        new-self (assoc moved-self :emits new-emits)
        gameobjs (if should-be-destroyed
                   new-particles
                   (conj new-particles new-self))]
    gameobjs))

(defmethod draw-obj :particle
  [self]
  (let [t (compute-particle-t self)
        fill ((:fill-fn self) t)
        stroke ((:stroke-fn self) t)
        line-width ((:line-width-fn self) t)
        scale ((:scale-fn self) t)
        scaled-self (rect-scale self scale)
        pos (:pos scaled-self)
        size (:size scaled-self)]
    [{:kind :rect :pos pos :size size :fill fill :stroke stroke :line-width line-width}]))



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
    [(make-particle (-> self decompose-rect :center) pfx-paddle-destroyed)]
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
  [self resources _input dt]
  (cond-> self
    (= (:game-state resources) :game-win) (assoc :vel (v* (:vel self) (- 1.0 (* 0.6 dt))))
    (not= (:game-state resources) :game-win) (assoc :vel (v* (v-normalize (:vel self)) ball-speed))
    true (assoc :pos (v+ (:pos self) (v* (:vel self) dt)))
    true (keep-in-bounds bounds-rect)))

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
        new-pos [old-x tweened-y]
        should-show-attack-particle (and (-> new-pos get-y (> (:bottom (decompose-rect bounds-rect))))
                                         (nil? (:has-attacked self)))
        new-self (cond-> self
                   true (assoc :descent-tween descent-tween)
                   true (assoc :pos new-pos)
                   should-show-attack-particle (assoc :has-attacked true))
        particle-pos (v+ (-> new-self decompose-rect :center) [0 100])
        gameobjs (if should-show-attack-particle
                   [new-self (make-particle particle-pos pfx-brick-attack)]
                   [new-self])]
    gameobjs))

(defmethod draw-obj :brick
  [self]
  {:kind :rect :pos (:pos self) :size (:size self) :fill "green" :stroke "darkgreen"})

(defmethod on-collision :brick
  [self _]
  (let [center (-> self decompose-rect :center)]
    (set! money (+ money 1))
    [(make-particle center pfx-brick-destroyed)]))



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

(defn disabled? [button]
  (= (:ui-state button) :disabled))

(defn button-clicked? [button-tag gameobjs]
  (some #(and (tagged-with? % button-tag) (clicked? %) (not (disabled? %))) gameobjs))

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
    (if (disabled? self)
      self
      (cond
        is-clicked (assoc self :ui-state :clicked)
        is-down (assoc self :ui-state :down)
        is-hovered (assoc self :ui-state :hovered)
        :else (assoc self :ui-state nil)))))

(defmethod draw-obj :button
  [self]
  (let [{pos :pos size :size text-pos-offset :text-pos-offset} (compute-text-rect! (:text self) (:font self) (:pos self))
        {rect-pos :pos rect-size :size} (rect-grow-centered {:pos pos :size size} (:h-padding self) (:v-padding self))
        text-pos (v+ pos text-pos-offset)
        color (cond
                (disabled? self) "#444"
                (down? self) "#222"
                (hovered? self) "#555"
                :else "#333")
        text-color (if
                    (disabled? self)
                     "#aaa"
                     "white")]
    [{:kind :rect :pos rect-pos :size rect-size :fill color :stroke text-color}
     {:kind :text :pos text-pos :text (:text self) :font (:font self) :fill text-color}]))



;; Game UI

(defn make-ui-timer []
  {:kind :ui-timer})

(defmethod tick-obj :ui-timer
  [self _resources _input _dt]
  self)

(defmethod draw-obj :ui-timer
  [_]
  (let [seconds (floor (rem game-time 60))
        minutes (quot game-time 60)
        ss (if (< seconds 10)
             (str "0" seconds)
             (str seconds))
        mm (if (< minutes 10)
             (str "0" minutes)
             (str minutes))
        text (str mm ":" ss)]
    [{:kind :text :text text :pos [80 60] :font "36px sans-serif" :line-width 1 :fill "white" :stroke "grey"}]))



(defn make-upgrade-button [pos text tag disabled]
  (cond-> (make-button pos text tag)
    true (assoc :font "16px sans-serif")
    true (assoc :h-padding 20)
    true (update :tags conj :upgrade-button)
    disabled (assoc :ui-state :disabled)))
