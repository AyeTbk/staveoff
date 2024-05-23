(ns staveoff.state)


(defonce mgrs [])
(defonce gameobjs [])
(defonce resources {})

(defonce bounds-rect {:pos [0 0] :size [0 0]})
(defonce ui-width 250)

(defonce brick-width 45)
(defonce brick-height 20)
(defonce brick-margin 5)

(defonce ball-speed-upgrade-increment 50)

;; Resetable state
(defonce automatic-descent-active false)
(defonce automatic-descent-delay 3)
(defonce descent-request-count 0)
(defonce descent-animation-duration 0.25)
(defonce game-over-animation-duration 1)
(defonce ball-speed 200)

(defn reset-state! []
  (set! automatic-descent-active false)
  (set! automatic-descent-delay 3)
  (set! descent-request-count 0)
  (set! descent-animation-duration 0.25)
  (set! game-over-animation-duration 1)
  (set! ball-speed 200))
