(ns staveoff.manager
  (:require
   [numb.math :refer [get-y]]
   [numb.time :refer [make-timer tick-timer]]
   [numb.prelude :as numb]
   [staveoff.state :refer [descent-request-count descent-animation-duration
                           automatic-descent-active automatic-descent-delay]]
   [staveoff.gameobj :refer [make-paddle make-ball make-brick tagged-with?
                             make-button clicked?]]))


;; Return a vector of updated [mgr gameobjs resources].
(defmulti tick-mgr (fn [mgr _gameobjs _resources _input _dt] (:kind mgr)))
(defmulti draw-mgr (fn [mgr _gameobjs _resources] (:kind mgr)))

(defmethod tick-mgr :default [self _ _ _]
  (throw (js/Error. (str "tick-mgr unimplemented for " self))))
(defmethod draw-mgr :default [_ _ _]
  [])



;; Game manager

(def menu-text {:kind :text :pos [0 0] :text "" :font "32px sans-serif" :fill "white"})
(def menu-title (merge menu-text {:font "56px sans-serif"}))
(def menu-small-text (merge menu-text {:font "18px sans-serif"}))

(defn make-game-manager []
  {:kind :game-manager})

(defmethod tick-mgr :game-manager
  [self gameobjs resources _input dt]
  (case (:game-state resources)

    nil
    (let [new-gameobjs [(make-button [300 330] "Start!" :btn-start)]
          new-resources (assoc resources :game-state :main-menu)]
      [self new-gameobjs new-resources])

    :main-menu
    (let [should-start-game (some #(and (tagged-with? % :btn-start) (clicked? %)) gameobjs)]
      (if should-start-game
        (let [new-gameobjs [(make-paddle)]
              new-resources {:game-state :game-start}]
          (set! descent-request-count 8)
          (set! automatic-descent-active true)
          [self new-gameobjs new-resources])
        [self gameobjs resources]))

    :game-start
    (if (= descent-request-count 0)
      (let [new-resources (assoc resources :game-state :game)
            new-gameobjs (vec (concat gameobjs [(make-ball)]))]
        [self new-gameobjs new-resources])
      [self gameobjs resources])

    :game
    (let [paddle-exists (true?
                         (some #(= (:kind %) :paddle)
                               gameobjs))
          end-of-screen (get-y (numb/canvas-size))
          ;; TODO Do I really want this lose condition?
          brick-got-past (true?
                          (some #(and (= (:kind %) :brick) (>= (-> % :pos get-y) end-of-screen))
                                gameobjs))
          game-over? (or brick-got-past (not paddle-exists))
          new-resources (if game-over?
                          (-> resources
                              (assoc :game-state :game-over-start)
                              (assoc :game-over-animation-timer (make-timer 2)))
                          resources)]
      (when game-over?
        (set! automatic-descent-active false))
      [self gameobjs new-resources])

    :game-over-start
    (let [[new-timer animation-is-over] (tick-timer (:game-over-animation-timer resources) dt)
          new-resources (cond-> resources
                          true (assoc :game-over-animation-timer new-timer)
                          animation-is-over (assoc :game-state :game-over))
          new-gameobjs (cond-> gameobjs
                         animation-is-over (conj (make-button [290 330] "Try again" :btn-restart)))]
      [self new-gameobjs new-resources])

    :game-over
    (let [should-restart-game (some #(and (tagged-with? % :btn-restart) (clicked? %)) gameobjs)
          new-resources (if should-restart-game
                          (assoc resources :game-state nil)
                          resources)]
      [self gameobjs new-resources])

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
     (merge menu-small-text {:pos [150 260] :text "If a brick gets past your paddle, it's game over."})]
    :game-start
    []
    :game-over
    [(merge menu-title {:pos [220 220] :text "Game over"})]
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
