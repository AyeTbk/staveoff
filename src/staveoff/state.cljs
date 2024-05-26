(ns staveoff.state
  (:require [numb.math :refer [v* randf rand-dir lerp ease-out ease-in]]
            [clojure.math :refer [floor]]))


(defonce mgrs [])
(defonce gameobjs [])
(defonce resources {})

(defonce bounds-rect {:pos [0 0] :size [0 0]})
(defonce ui-width 250)

(defonce brick-width 45)
(defonce brick-height 20)
(defonce brick-margin 5)

(defonce ball-speed-upgrade-increment 50)

(defonce expected-game-duration (* 5 60))
(defonce game-start-descent-request-count 7)
(defonce automatic-descent-delay-max 5)
(defonce automatic-descent-delay-min 0.3)

;; Resetable state
(defonce automatic-descent-active false)
(defonce automatic-descent-delay automatic-descent-delay-max)
(defonce descent-request-count 0)
(defonce descent-animation-duration 0.25)
(defonce game-over-animation-duration 1)
(defonce game-win-animation-duration 2)
(defonce game-time 0)
(defonce ball-speed 300)

(defn reset-state! []
  (set! automatic-descent-active false)
  (set! automatic-descent-delay automatic-descent-delay-max)
  (set! descent-request-count 0)
  (set! descent-animation-duration 0.25)
  (set! game-over-animation-duration 1)
  (set!  game-win-animation-duration 2)
  (set! game-time 0)
  (set! ball-speed 300))


;; Particle fx

(def pfx-spawner
  {:life-span #(identity 0.001)
   :start-size #(vec [1 1])
   :start-vel #(vec [0 0])
   :fill #(identity nil)
   :stroke #(identity nil)
   :line-width #(identity 0)
   :scale #(identity 0)
   :gravity #(identity [0 0])
   :emits []})

(def pfx-paddle-destroyed
  (let [pfx-paddle-particle {:life-span #(randf 0.05 0.2)
                             :start-size #(vec [5 5])
                             :start-vel #(v* (rand-dir 90 360) (randf 250 550))
                             :fill (fn [_t] "lightgrey")
                             :stroke (fn [_t] "grey")
                             :line-width (fn [_t] 2)
                             :scale (fn [t] (lerp 2.0 0.0 t))
                             :gravity (fn [_t] [0 2100])
                             :emits []}]
    (merge pfx-spawner {:life-span #(identity 1.0)
                        :emits [{:count #(randf 14 17)
                                 :time 0.5
                                 :span 1.0
                                 :desc pfx-paddle-particle}]})))

(def pfx-brick-destroyed
  (let [pfx-brick-particle {:life-span #(randf 0.05 0.2)
                            :start-size #(vec [5 5])
                            :start-vel #(v* (rand-dir 90 360) (randf 250 350))
                            :fill (fn [_t] "green")
                            :stroke (fn [_t] "darkgreen")
                            :line-width (fn [_t] 3)
                            :scale (fn [t] (lerp 1.5 0.0 t))
                            :gravity (fn [_t] [0 2100])
                            :emits []}]
    (merge pfx-spawner {:emits [{:count #(randf 4 7)
                                 :time 0.5
                                 :span 1.0
                                 :desc pfx-brick-particle}]})))

(def pfx-brick-attack
  (let [pfx-attack-particle {:life-span #(identity 1.0)
                             :start-size #(vec [2 15])
                             :start-vel #(v* (rand-dir 270 10) (randf 1050 2550))
                             :fill (fn [_t] "yellow")
                             :stroke (fn [_t] "red")
                             :line-width (fn [_t] 1)
                             :scale (fn [t] (lerp 1.0 0.0 t))
                             :gravity (fn [_t] [0 4100])
                             :emits []}]
    (merge pfx-spawner {:life-span #(randf 0.15 0.3)
                        :emits [{:count #(randf 10 20)
                                 :time 0.5
                                 :span 1.0
                                 :desc pfx-attack-particle}]})))

(def pfx-firework
  (let [pfx-smoke {:life-span #(randf 0.25 0.5)
                   :start-size #(vec [5 5])
                   :start-vel #(v* (rand-dir 270 5) -100)
                   :fill (fn [_t] "rgb(200 200 200 / .5)")
                   :stroke (fn [_t] nil)
                   :line-width (fn [_t] 1)
                   :scale (fn [t] (ease-in (randf 2.0) 0.0 t))
                   :gravity (fn [_t] [0 -50])
                   :emits []}
        pfx-stars {:life-span #(randf 0.6 1.6)
                   :start-size #(vec [5 5])
                   :start-vel #(v* (rand-dir 0 360) (randf 140 220))
                   :fill (fn [t] (let [hsl (str "hsl(" (floor (* t 360)) " 100 50)")]
                                   hsl))
                   :stroke (fn [_t] nil)
                   :line-width (fn [_t] 1)
                   :scale (fn [t] (ease-in 1.0 0.0 t))
                   :gravity (fn [_t] [0 200])
                   :emits [{:count #(randf 5 10)
                            :time 0.9
                            :span 0.2
                            :desc pfx-smoke}]}
        pfx-lift {:life-span #(randf 0.6 0.9)
                  :start-size #(vec [10 10])
                  :start-vel #(v* (rand-dir 270 45) (randf 450 950))
                  :fill (fn [_t] "white")
                  :stroke (fn [_t] "lightgrey")
                  :line-width (fn [_t] 1)
                  :scale (fn [t] (ease-out 1.0 0.5 t))
                  :gravity (fn [_t] [0 800])
                  :emits [{:count #(randf 20 25)
                           :time 0.5
                           :span 1.0
                           :desc pfx-smoke}
                          {:count #(randf 21 28)
                           :time 0.98
                           :span 0.01
                           :desc pfx-stars}]}]
    (merge pfx-spawner {:life-span #(identity 100)
                        :emits [{:count #(identity 100)
                                 :time 0.51
                                 :span 1.0
                                 :desc pfx-lift}]})))
