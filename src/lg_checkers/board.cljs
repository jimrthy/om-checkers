(ns lg-checkers.board
  "This is where the business logic lives"
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [schema.core :as s :refer-macros [defn]]))

;;;; core.async is used to implement CSP (Communicating
;;;; Sequential Proceses), and channels are used to report
;;;; user interaction events, as well as changing the board
;;;; state.

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
(def board-repr {:playing-field {s/Int pieces}
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

; == Internals ==============================================

(enable-console-print!)

;;; Namespace level vars like this are, at best, a
;;; code smell.
;;; TODO: Come up with a better approach
(defonce event-stack (atom []))
(declare board board-commands board-events)

; === Utility Functions =================================

; positional constants
(def top-row 1)
(def bottom-row 8)

; == Board State ==========================================
; initialize a board, where positions are indexed 1-32.
; each position is an atom containing the symbol of the
; piece in it.
(s/defn create-board :- board-repr []
  (atom
   {:playing-field (apply sorted-map
                          (flatten
                           (map-indexed (fn [i v] (vector (inc i) v))
                                        (flatten
                                         [(repeat 12 :red-piece)
                                          (repeat 8 :empty-piece)
                                          (repeat 12 :black-piece)]))))
    :blacks-turn? true
    ;; When a player clicks on his piece, it's the first half
    ;; of moving it. Keep track of which piece they've told
    ;; us they want to move here
    :piece-to-move nil}))

(defn reset
  "Start over with a clean slate"
  []
  (swap! board (fn [_] @(create-board)))
  (swap! event-stack (fn [_] [])))

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
  (throw (ex-info "Not Implemented" {:problem "Get this written"})))

(defmethod board-update :select
  [cmd current]
  (throw (ex-info "Not Implemented" {:problem "Get this written"})))

; given a board position, return the position of neighbors
; [NOTE:] Challengee should investigate memoization of
;         this function.
(s/defn compute-pos-neighbors :- [s/Int]
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

(s/def neighbor-pos
  "Memoized version of compute-pos-neighbors"
  (memoize compute-pos-neighbors))

; compute neighbors for every board position
(defn compute-neighbor-positions []
  (map (fn [pos] {pos (neighbor-pos pos)})
       (range 1 33)))
;; Q:  Is it worth delaying startup to get these
;; cached?
;; A: For something this small and simple, it could
;; go either way.
;; For a real scenario, it would probably be wise
;; to just run this in a go block or have the server
;; compute it.
;; For that matter, it might be best to do something
;; like this as a macro that expands to a
;; hashmap that we can look up directly.
(compute-neighbor-positions)

(s/defn red-event->command :- (s/maybe command-event)
  [game-state
   ev :- request-event]
  (println "It's red's turn")
  (throw (ex-info "Not Implemented"
                  {:problem "This would almost be a direct copy/paste from the black event version"})))

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

(s/defn event->command :- (s/maybe command-event)
  "Translate an incoming request into an outgoing command.
Or nil, if the request isn't legal."
  [ev :- request-event]
  (println "Applying business rules to the event")
  (let [game-state @board]
    (if (:blacks-turn? game-state)
      (black-event->command game-state ev)
      (red-event->command game-state ev))))

; == Concurrent Processes =================================

; the board receives commands to manipulate its state
;     {:command :command-symbol
;      :position <integer>
;      :piece :piece-symbol}

(def board-commands (chan))

;; this concurrent process reacts to board click events --
;; at present, it sets the board position clicked to contain
;; a black piece by sending a command to the board-commands
;; channel

(go (while true
      (let [event (<! board-events)]
        (print "UI event loop: " event)
        ;; Save all the incoming events to
        ;; make the playback more realistic
        (swap! event-stack conj event)
        (when-let [command (event->command event)]
          (println "Sending command to update the game")
          (put! board-commands command)))))

; this concurrent process receives board command messages
; and executes on them.  at present, the only thing it does
; is sets the desired game position to the desired piece
(go (while true
      (let [command (<! board-commands)]
        (print "Command event loop: " command)
        ;; Q: Is it more idiomatic to update the state this way or using om/transact!
        ;; A: It seems like it would be really silly to use the
        ;; View implementation way out here in the business logic side
        ;; of things.
        (swap! board (partial board-update command)
               (fn [old]
                 (assoc-in old [:playing-field (:position command)]
                           (:piece command))))
        (when (game-over? @board)
          ;; TODO: Congratulate the winner
          (reset)))))

; == Public ===========================================

; ===Channels ===========================================
; the board generates events on this channel
;     {:event :event-symbol
;      :position <int>}
(def board-events (chan))

; for other processes to acquire the board state atom
;     (atom (create-board))
(def board-state (chan))

;; Instantiate our game board state, initializing our
;; board with starting pieces
;; TODO: This should be a defonce so we don't lose
;; state every time I save a change
(def board (create-board))

