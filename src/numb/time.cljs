(ns numb.time
  (:require [numb.math :refer [clamp ease-out]]))

(defn make-timer [duration & options]
  (let [options (set options)]
    {:duration duration
     :elapsed 0
     :cyclic (contains? options :cyclic)}))

(defn tick-timer [timer dt]
  (if (<= (:duration timer) 0)
    [timer 1]
    (let [duration (:duration timer)
          old-elapsed (:elapsed timer)
          elapsed (+ old-elapsed dt)
          trigger-count (if (:cyclic timer)
                          (quot elapsed duration)
                          (if (and (< old-elapsed duration) (>= elapsed duration))
                            1
                            0))
          new-elapsed (if (:cyclic timer)
                        (mod elapsed duration)
                        elapsed)
          new-timer (assoc timer :elapsed new-elapsed)
          triggered (if (= trigger-count 0)
                      nil
                      trigger-count)]
      [new-timer triggered])))


(defn make-tween [start end duration]
  {:start start
   :end end
   :duration duration
   :elapsed 0})

(defn tick-tween [tween dt]
  (let [elapsed (+ (:elapsed tween) dt)
        finished (>= elapsed (:duration tween))
        t (clamp (/ elapsed (:duration tween)) 0 1)
        tweened-value (ease-out (:start tween) (:end tween) t)
        new-tween (assoc tween :elapsed elapsed)]
    [new-tween tweened-value finished]))
