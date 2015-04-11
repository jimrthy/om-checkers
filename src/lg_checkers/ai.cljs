(ns lg-checkers.ai
  "The computer-controlled opponent

For single-player games (TODO: add a way to toggle that),
we need a go loop to analyze the game state after the player
moves and put appropriate events on the board-events channel
to simulate the input from another player."
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! chan <! >!]]
            [lg-checkers.board :as board]
            [schema.core :as s :refer-macros [defn]]))

; == Schema =========================================

;; I'm using this in 2 namespaces now
;; TODO: Refactor it into a 3rd they can share.
;; Something like utils
(def async-channel (type (chan)))
(def atom-type (type (atom nil)))
(def challenge-level (s/enum :beginner :easy :medium :hard :genius))

; == Internals ======================================

(s/defn watch-for-my-turn
  [notifier :- async-channel
   playing-black? :- s/Bool
   key
   a
   old-val
   new-val]
  (let [blacks-turn? (-> new-val :rules :blacks-turn?)]
    (when (= blacks-turn? playing-black?)
      ;; It's my move. Signal the analyzer that it's time
      ;; to do its thing
      (>! notifier (:playing-field new-val)))))

(s/defn analyzer
  "Examine the game state. Watch for changes. Plan ahead.
Notify the event loop about the computer's next move"
  [board-events :- async-channel
   playing-black? :- s/Bool
   state-change :- async-channel
   difficulty :- challenge-level]
  (go
    ;; TODO: Add the ability to start from any state
    ;; instead
    (loop [current-state (board/clean-slate)]
      (when current-state
        ;; TODO: This is where all the interesting
        ;; parts really need to happen.
        ;; For a baseline, we could just examine
        ;; the current state, pick a random legal
        ;; move, and put events onto the board-events
        ;; channel to simulate the input from another
        ;; player.
        (recur (<! state-change))))))

; == Public =========================================

(defn ctor
  [board-events :- async-channel
   game-state :- atom-type
   playing-black? :- s/Bool
   difficulty :- challenge-level]
  (let [state-change (chan)]
    (add-watch game-state :turn-changer
               (partial watch-for-my-turn state-change playing-black?))
    (analyzer board-events playing-black? state-change difficulty)))
