(ns lg-checkers.board
  "This is where the business logic lives"
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [schema.core :as s :refer-macros [defn]]))

;;;; core.async is used to implement CSP (Communicating
;;;; Sequential Proceses), and channels are used to report
;;;; user interaction events, as well as changing the board
;;;; state.

(enable-console-print!)

; == Schema ==================================================

;; These map to entries in checkers.css
(def pieces (s/enum :empty
                    :black-piece
                    :prom-black-piece
                    :red-piece
                    :prom-red-piece))

; The board is laid out as a 32 element map, one element
; for each position.  It is stored in an atom, and bound
; to the UI.  Any update of the atom will cause an UI
; refresh to reflect the current board state.
(def board-repr {:playing-field [[pieces]]
                 :blacks-turn? s/Bool})

;;; Another namespace is responsible for converting DOM
;;; (real or simulated) events into request-events
(def input-events (s/enum :board-clicked))
(def request-event {:event input-events
                    :position s/Int})
;;; It's our responsibility to translate those into
;;; command-events and cope with them (legal or not)
(def legal-commands (s/enum :update-board-position))
(def command-event {:command legal-commands
                    :position s/Int
                    :piece pieces})

(defmulti board-update
  "Return the appropriate new state of the board after applying command"
  (fn [command old-board-state]
    (:command command)))

;; This will get defined in the system constructor
(declare reset)

;;; == Internals ==============================================

(def event-stack
  "Track event history

Top-level defs are bad.
TODO: Hide this elsewhere."
  (atom []))

; === Utility Functions =================================

; positional constants
(def top-row 1)
(def bottom-row 8)

; == Board State ==========================================
(s/defn clean-slate :- board-repr
  "Set up the beginning of the game"
  []
  {:playing-field (mapv #(mapv vector %)
                        (partition 4 (concat (repeat 12 :red-piece) (repeat 8 :empty-piece) (repeat 12 :black-piece))))
   
   :blacks-turn? true

   ;; When a player clicks on his piece, it's the first half
   ;; of moving it. Keep track of which piece they've told
   ;; us they want to move here
   :piece-to-move nil})

(s/defn game-over? :- s/Bool
  [game-state]
  (let [contents (-> game-state :playing-field vals)]
    ;; TODO: Also have to check that the current player has
    ;; a legal move available
    (and (some #(or (= :black-piece %)
                    (= :prom-black-piece %)) contents)
         (some #(or (= :red-piece %) (= :prom-red-piece %)) contents))))

(defmethod board-update :move
  [cmd current]
  (let [updated nil]
    (throw (ex-info "Not Implemented" {:problem "Get this written"}))
    (when (game-over? updated)
      ;; TODO: Congratulate the winner
      (reset))
    updated))

(defmethod board-update :select
  [cmd current]
  (throw (ex-info "Not Implemented" {:problem "Get this written"})))

(s/defn compute-pos-neighbors :- [s/Int]
  "given a board position, return the position of neighbors

Note that this is totally independent of the actual board.
And very tied in to the original representation as a flat vector.
=> current implementation is now broken"
  [pos :- s/Int]
  (let [curr-row (Math/ceil (/ pos 4))
        row-odd? (odd? curr-row)
        row-even? (not row-odd?)
        top-row? (= curr-row top-row)
        bottom-row? (= curr-row bottom-row)
        right-edge? (= (mod pos 4) 0)
        left-edge? (= (mod pos 4) 1)
        up-left (if row-odd? (- pos 4)
                             (- pos 5))
        up-right (if row-odd? (- pos 3)
                              (- pos 4))
        down-left (if row-odd? (+ pos 4)
                               (+ pos 5))
        down-right (if row-odd? (+ pos 3)
                                (+ pos 4))]
    (remove nil?
            (flatten
             [(if (not top-row?)
                (if row-even?
                  [up-left up-right]
                  [(if (not left-edge?)
                     up-left)
                   (if (not right-edge?)
                     up-right)]))
              (if (not bottom-row?)
                (if row-odd?
                  [down-left down-right]
                  [(if (not left-edge?)
                     down-left)
                   (if (not right-edge?)
                     down-right)]))]))))

(def neighbor-pos
  "Memoized version of compute-pos-neighbors"
  (memoize compute-pos-neighbors))

(defn compute-neighbor-positions
  "compute neighbors for every board position

Mainly for the sake of getting the memoization primed"
  []
  (map (fn [pos] {pos (neighbor-pos pos)})
       (range 1 33)))

;;; These next approaches are wrong.
;;; TODO: Pull these from my rebuild branch instead
(s/defn red-event->command :- (s/maybe command-event)
  [game-state
   ev :- request-event]
  (println "It's red's turn")
  (throw (ex-info "Not Implemented"
                  {:problem "This would almost be a direct copy/paste from the black event version"})))

(comment (def board-commands (chan)))
(comment (def board-events (chan)))
(comment (def board-state (chan)))
(comment (def board (create-board)))

(s/defn black-event->command :- (s/maybe command-event)
  [game-state ev :- request-event]
  ;; Will potentially return things like
  #_{:command :update-board-position
     :position (:position event)
     :piece :black-piece}
  (println "Black's move:\n" (pr-str ev) "\non\n" (pr-str game-state))
  (let [result
        (let [cell-index (:position ev)]
          (println "Where's our playing field?")
          (let [playing-field (:playing-field game-state)]
            (println "What's in cell " cell-index "where the click happened?")
            (let [clicked-contents (playing-field cell-index)]
              (println "Clicked on" (pr-str clicked-contents) "\nIs this the second half of an attempted move?")
              (let [starting-index (:piece-to-move game-state)]
                (println "Analyzing move")
                (cond (and starting-index
                           (= clicked-contents :empty))
                      {:command :move
                       :from starting-index
                       :to cell-index}
                      (or (= clicked-contents :black-piece)
                          (= clicked-contents :prom-black-piece))
                      {:command :select
                       :at cell-index}
                      :else (println "Illegal event attempted"))))))]
    (println "event->command => " (pr-str result))
    result))

;;; == Public ================================================

(s/defn event->command :- (s/maybe command-event)
  "Translate an incoming request into an outgoing command.
Or nil, if the request isn't legal."
  [game-state :- board-repr
   ev :- request-event]
  (println "Applying business rules to the event")
  ;; Save all the incoming events to
  ;; make the playback more realistic
  (swap! event-stack conj ev)

  (if (:blacks-turn? game-state)
    (black-event->command game-state ev)
    (red-event->command game-state ev)))

