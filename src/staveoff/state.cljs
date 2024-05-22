(ns staveoff.state)


(defonce mgrs [])
(defonce gameobjs [])
(defonce resources {})

(defonce automatic-descent-active false)
(defonce automatic-descent-delay 3)
(defonce descent-request-count 0)
(defonce descent-animation-duration 0.25)

(defonce ball-paddle-bounce-accel 1.015)
