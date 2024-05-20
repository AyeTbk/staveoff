(ns staveoff.manager
  (:require
   [numb.time :refer [make-timer tick-timer]]
   [staveoff.gameobj :refer [make-paddle make-ball make-brick
                             descent-animation-duration]]))


;; Return a vector of updated [mgr gameobjs resources].
(defmulti tick-mgr (fn [mgr _gameobjs _resources _input _dt] (:kind mgr)))
(defmulti draw-mgr (fn [mgr _gameobjs _resources] (:kind mgr)))

(defmethod tick-mgr :default [self _ _ _]
  (throw (js/Error. (str "tick-mgr unimplemented for " self))))
(defmethod draw-mgr :default [_ _ _]
  [])


(defonce automatic-descent-active false)
(defonce automatic-descent-delay 3)
(defonce descent-request-count 0)


;; Game manager

;; TODO
;; = game =
;; to be determined
;; = lose condition / defeat screen =
;; When paddle touches a brick (potential for health upgrade?) or a brick gets past you, game over.
(def menu-text {:kind :text :pos [0 0] :text "" :font "32px sans-serif" :fill "white"})
(def menu-title (merge menu-text {:font "48px sans-serif"}))
(def menu-small-text (merge menu-text {:font "18px sans-serif"}))

(defn make-game-manager []
  {:kind :game-manager})

(defmethod tick-mgr :game-manager
  [self gameobjs resources input _dt]
  (case (:game-state resources)
    nil
    (let [new-resources (assoc resources :game-state :main-menu)]
      [self gameobjs new-resources])
    :main-menu
    (if (-> input :mouse :just-pressed (contains? :left))
      (let [new-gameobjs [(make-paddle)]
            new-resources {:game-state :game-start}]
        (set! descent-request-count 8)
        (set! automatic-descent-active true)
        [self new-gameobjs new-resources])
      [self gameobjs resources])
    :game-start
    (if (= descent-request-count 0)
      (let [new-resources (assoc resources :game-state :game)
            new-gameobjs (vec (concat gameobjs [(make-ball)]))]
        [self new-gameobjs new-resources])
      [self gameobjs resources])
    ;else
    [self gameobjs resources]))

(defmethod draw-mgr :game-manager
  [_ _ resources]
  (case (:game-state resources)
    :main-menu
    [(merge menu-title {:pos [150 100] :text "Stave Off"})
     (merge menu-small-text {:pos [150 150] :text "Survive until the end."})
     (merge menu-small-text {:pos [150 180] :text "The only threat is the onslaught of bricks."})
     (merge menu-small-text {:pos [150 230] :text "If a brick hits your paddle, it's game over."})
     (merge menu-small-text {:pos [150 260] :text "If a brick gets past your paddle, it's game over."})
     (merge menu-text {:pos [150 350] :text "Left click" :fill "red"})
     (merge menu-text {:pos [290 350] :text "to start" :fill "white"})]
    :game-start
    []
    ;else
    []))


;; Brick manager

(defn make-brick-manager []
  {:kind :brick-manager
   :automatic-descent-timer (make-timer 0 :cyclic)
   :requested-descent-timer (make-timer 0 :cyclic)})

(defmethod tick-mgr :brick-manager
  [self gameobjs resources _input dt]
  (if (contains? #{:game-start :game} (:game-state resources))
    (let [[should-tick-auto-timer should-tick-requ-timer] (if (> descent-request-count 0)
                                                            [false true]
                                                            [automatic-descent-active false])
          [auto-timer auto-descent-triggered] (if should-tick-auto-timer
                                                (tick-timer (:automatic-descent-timer self) dt)
                                                [(:automatic-descent-timer self) false])
          new-auto-timer (if auto-descent-triggered
                           (-> auto-timer
                               (assoc :duration automatic-descent-delay)
                               (assoc :elapsed 0))
                           auto-timer)
          [requ-timer requ-descent-triggered] (if should-tick-requ-timer
                                                (tick-timer (:requested-descent-timer self) dt)
                                                [(:requested-descent-timer self) false])
          new-requ-timer (if requ-descent-triggered
                           (-> requ-timer
                               (assoc :duration descent-animation-duration)
                               (assoc :elapsed 0))
                           requ-timer)
          new-self (-> self
                       (assoc :automatic-descent-timer new-auto-timer)
                       (assoc :requested-descent-timer new-requ-timer))
          descent-triggered (or auto-descent-triggered requ-descent-triggered)
          spawned-bricks (if descent-triggered
                           (vec (for [x (range 10)]
                                  (make-brick [(+ (* x (+ 45 5)) 120) -25])))
                           [])
          new-gameobjs (vec (concat gameobjs spawned-bricks))
          new-resources (if descent-triggered
                          (assoc resources :descend-i-command-you! true)
                          (assoc resources :descend-i-command-you! false))]
      (when requ-descent-triggered
        (set! descent-request-count (- descent-request-count 1)))
      [new-self new-gameobjs new-resources])
    ;else
    [self gameobjs resources]))
