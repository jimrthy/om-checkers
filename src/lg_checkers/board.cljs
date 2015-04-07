(ns lg-checkers.board
  "This is where the business logic lives"
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! chan <!]]
            [schema.core :as s :refer-macros [defn]]))

;;;; core.async is used to implement CSP (Communicating
;;;; Sequential Proceses), and channels are used to report
;;;; user interaction events, as well as changing the board
;;;; state.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals

(enable-console-print!)

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
    :blacks-turn? true}))

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

; == Concurrent Processes =================================

; the board receives commands to manipulate its state
;     {:command :command-symbol
;      :position <integer>
;      :piece :piece-symbol}

(def board-commands (chan))

; this concurrent process reacts to board click events --
; at present, it sets the board position clicked to contain
; a black piece by sending a command to the board-commands
; channel
(declare board-events)
(go (while true
      (let [event (<! board-events)]
        (put! board-commands
              {:command :update-board-position
               :position (:position event)
               :piece :black-piece}))))

; this concurrent process receives board command messages
; and executes on them.  at present, the only thing it does
; is sets the desired game position to the desired piece
(declare board board-commands)
(go (while true
      (let [command (<! board-commands)]
        (swap! board assoc (:position command)
                           (:piece command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public

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

