(ns staveoff.manager
  (:require
   [numb.math :refer [get-x get-y decompose-rect clamp lerp]]
   [numb.time :refer [make-timer tick-timer]]
   [numb.prelude :as numb]
   [staveoff.state :refer [reset-state!
                           descent-request-count descent-animation-duration
                           automatic-descent-active automatic-descent-delay
                           game-over-animation-duration game-win-animation-duration
                           bounds-rect brick-width brick-height brick-margin
                           expected-game-duration
                           game-time automatic-descent-delay-max automatic-descent-delay-min
                           game-start-descent-request-count
                           ball-speed ball-speed-upgrade-increment
                           pfx-firework
                           money faster-ball-prices more-ball-prices]]
   [staveoff.gameobj :refer [make-paddle make-ball make-brick
                             make-ui-timer make-upgrade-button
                             make-button button-clicked?
                             make-particle]]))


;; Return a vector of updated [mgr gameobjs resources].
(defmulti tick-mgr (fn [mgr _gameobjs _resources _input _dt] (:kind mgr)))
(defmulti draw-mgr (fn [mgr _gameobjs _resources] (:kind mgr)))

(defmethod tick-mgr :default [self _ _ _]
  (throw (js/Error. (str "tick-mgr unimplemented for " self))))
(defmethod draw-mgr :default [_ _ _]
  [])



;; Game manager

(def menu-text {:kind :text :pos [0 0] :text "" :font "32px sans-serif" :fill "white" :stroke "black"  :line-width 0.5})
(def menu-title (merge menu-text {:font "56px sans-serif" :line-width 2}))
(def menu-small-text (merge menu-text {:font "18px sans-serif" :stroke nil}))

(defn make-game-manager []
  {:kind :game-manager})

(defmethod tick-mgr :game-manager
  [self gameobjs resources _input dt]
  (case (:game-state resources)

    nil
    (let [new-gameobjs [(make-button [300 330] "Start!" :btn-start)]
          new-resources (assoc resources :game-state :main-menu)]
      (reset-state!)
      [self new-gameobjs new-resources])

    :main-menu
    (let [should-start-game (button-clicked? :btn-start gameobjs)]
      (if should-start-game
        (let [new-gameobjs [(make-paddle)]
              new-resources {:game-state :game-start}]
          (set! descent-request-count (- game-start-descent-request-count 1))
          (set! automatic-descent-active true)
          [self new-gameobjs new-resources])
        [self gameobjs resources]))

    :game-start
    (if (= descent-request-count 0)
      (let [new-resources (assoc resources :game-state :game)
            new-gameobjs (vec (concat gameobjs [(make-ball) (make-ui-timer)]))]
        [self new-gameobjs new-resources])
      [self gameobjs resources])

    :game
    (let [paddle-exists (true?
                         (some #(= (:kind %) :paddle)
                               gameobjs))
          end-of-screen (get-y (numb/canvas-size))
          ;; TODO Do I really want this lose condition?
          brick-got-past (true?
                          (some #(and (= (:kind %) :brick) (>= (-> % :pos get-y) (- end-of-screen 10)))
                                gameobjs))
          game-over? (or brick-got-past (not paddle-exists))
          time-is-up (> game-time expected-game-duration)
          there-remains-bricks (some #(= (:kind %) :brick) gameobjs)
          game-won? (and time-is-up (not there-remains-bricks))
          new-resources (cond-> resources
                          game-over? (->
                                      (assoc :game-state :game-over-start)
                                      (assoc :game-over-animation-timer (make-timer game-over-animation-duration)))
                          game-won? (->
                                     (assoc :game-state :game-win-start)
                                     (assoc :game-win-animation-timer (make-timer game-win-animation-duration))))
          new-automatic-descent-delay (lerp
                                       automatic-descent-delay-max
                                       automatic-descent-delay-min
                                       (clamp (/ game-time expected-game-duration) 0 1))
          fireworks-pos (-> bounds-rect decompose-rect (#(vec [(-> % :center get-x) (:bottom %)])))
          new-gameobjs (if game-won?
                         (vec (concat gameobjs [(make-particle fireworks-pos pfx-firework)]))
                         gameobjs)]
      (set! game-time (+ game-time dt))
      (set! automatic-descent-delay new-automatic-descent-delay)
      (when game-over?
        (set! automatic-descent-active false))
      [self new-gameobjs new-resources])

    :game-over-start
    (let [[new-timer animation-is-over] (tick-timer (:game-over-animation-timer resources) dt)
          new-resources (cond-> resources
                          true (assoc :game-over-animation-timer new-timer)
                          animation-is-over (assoc :game-state :game-over))
          new-gameobjs (cond-> gameobjs
                         animation-is-over (conj (make-button [290 330] "Try again" :btn-restart)))]
      [self new-gameobjs new-resources])

    :game-over
    (let [should-restart-game (button-clicked? :btn-restart gameobjs)
          new-resources (if should-restart-game
                          (assoc resources :game-state nil)
                          resources)]
      [self gameobjs new-resources])

    :game-win-start
    (let [[new-timer animation-is-over] (tick-timer (:game-win-animation-timer resources) dt)
          new-resources (cond-> resources
                          true (assoc :game-win-animation-timer new-timer)
                          animation-is-over (assoc :game-state :game-win))
          new-gameobjs (cond-> gameobjs
                         animation-is-over (conj (make-button [290 330] "Play again" :btn-restart)))]
      [self new-gameobjs new-resources])

    :game-win
    (let [should-restart-game (button-clicked? :btn-restart gameobjs)
          new-resources (if should-restart-game
                          (assoc resources :game-state nil)
                          resources)]
      [self gameobjs new-resources])

    ;else
    [self gameobjs resources]))

(defmethod draw-mgr :game-manager
  [_ _ resources]
  (let [bounds (merge bounds-rect {:kind :rect :fill nil :stroke "grey" :line-width 1})]
    (case (:game-state resources)
      :main-menu
      [(merge menu-title {:pos [150 100] :text "Stave Off"})
       (merge menu-small-text {:pos [150 150] :text "Survive until the end (5 minutes)."})
       (merge menu-small-text {:pos [150 180] :text "The only threat is the onslaught of bricks."})
       (merge menu-small-text {:pos [150 230] :text "If a brick hits your paddle, it's game over."})
       (merge menu-small-text {:pos [150 260] :text "If a brick gets past your paddle, it's game over."})]
      :game-start
      [bounds]
      :game
      [bounds]
      :game-over-start
      [bounds]
      :game-over
      [bounds
       (merge menu-title {:pos [220 220] :text "Game over"})]
      :game-win-start
      [bounds]
      :game-win
      [bounds
       (merge menu-title {:pos [220 220] :text "You win!"})]
      ;else
      [])))



;; Upgrade manager

(defn make-upgrade-manager []
  {:kind :upgrade-manager})

(defmethod tick-mgr :upgrade-manager
  [self gameobjs resources _input _dt]
  (let [create-upgrade-buttons (fn []
                                 [(make-upgrade-button [30 310] "Faster ball" :btn-faster-balls
                                                       (< money (first faster-ball-prices)))
                                  (make-upgrade-button [30 360] "More balls" :btn-more-balls
                                                       (< money (first more-ball-prices)))])
        destroy-upgrade-buttons (fn [gameobjs]
                                  (vec
                                   (filter #(not (contains? (:tags %) :upgrade-button)) gameobjs)))]
    (case (:game-state resources)
      :game
      (let [new-gameobjs (vec (concat (destroy-upgrade-buttons gameobjs) (create-upgrade-buttons)))]
        (cond
          (button-clicked? :btn-faster-balls gameobjs)
          (do
            (set! ball-speed (+ ball-speed ball-speed-upgrade-increment))
            (set! money (- money (first faster-ball-prices)))
            (set! faster-ball-prices (if (second faster-ball-prices)
                                       (next faster-ball-prices)
                                       faster-ball-prices))
            [self new-gameobjs resources])

          (button-clicked? :btn-more-balls gameobjs)
          (let [new-new-gameobjs (vec (concat new-gameobjs [(make-ball)]))]
            (set! money (- money (first more-ball-prices)))
            (set! more-ball-prices (if (second more-ball-prices)
                                     (next more-ball-prices)
                                     more-ball-prices))
            [self new-new-gameobjs resources])

          :else
          [self new-gameobjs resources]))

      :game-over-start
      (let [new-gameobjs (destroy-upgrade-buttons gameobjs)]
        [self new-gameobjs resources])
      :game-win-start
      (let [new-gameobjs (destroy-upgrade-buttons gameobjs)]
        [self new-gameobjs resources])

      ;else
      [self gameobjs resources])))

(defmethod draw-mgr :upgrade-manager
  [_ _ resources]
  (case (:game-state resources)
    :game
    [(merge menu-text {:pos [20 270] :text "Upgrades"})
     (merge menu-small-text {:pos [20 220] :text (str "Money: $" money)})
     (merge menu-small-text {:pos [125 320] :text (str "$" (first faster-ball-prices))})
     (merge menu-small-text {:pos [125 370] :text (str "$" (first more-ball-prices))})]
    ;else
    []))



;; Brick manager

(defn make-brick-manager []
  {:kind :brick-manager
   :automatic-descent-timer (make-timer descent-animation-duration :cyclic)
   :requested-descent-timer (make-timer 0 :cyclic)})

(defmethod tick-mgr :brick-manager
  [self gameobjs resources _input dt]
  (cond
    (contains? #{:game-start :game} (:game-state resources))
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
          time-is-up (> game-time expected-game-duration)
          spawned-bricks (if (and descent-triggered (not time-is-up))
                           (vec
                            (for [i (range 7)]
                              (let [bounds (decompose-rect bounds-rect)
                                    half-brick-width (/ brick-width 2)
                                    x (- i 3)
                                    x (- (+ (-> bounds :center get-x) (* x (+ brick-width brick-margin))) half-brick-width)
                                    y (- (+ brick-height brick-margin))]
                                (make-brick [x y]))))
                           [])
          new-gameobjs (vec (concat gameobjs spawned-bricks))
          new-resources (if descent-triggered
                          (assoc resources :descend-i-command-you! true)
                          (assoc resources :descend-i-command-you! false))]
      (when requ-descent-triggered
        (set! descent-request-count (- descent-request-count 1)))
      [new-self new-gameobjs new-resources])

    (contains? #{:game-over-start} (:game-state resources))
    (let [new-resources (assoc resources :descend-i-command-you! true)] ; Animate all the bricks down on game-over, looks cool.
      [self gameobjs new-resources])

    :else
    [self gameobjs resources]))
