(ns lg-checkers.board
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

;;; == Globals =========================================

;;; Well, they aren't quite that evil. But they're worth
;;; calling out

(def game-state (atom {:blacks-turn? true  ; or is it red's?
                       ;; Has the player selected a piece to
                       ;; move next?
                       :pending-move nil}))

; == Notes ==============================================
; Board pieces are defined in the checkers.css file.  The
; currently defined pieces are:
;     :red-piece
;     :black-piece
;     :prom-red-piece
;     :prom-black-piece
;     :empty
;
; The board is laid out as a 32 element map, one element
; for each position.  It is stored in an atom, and bound
; to the UI.  Any update of the atom will cause an UI
; refresh to reflect the current board state.
;
; core.async is used to implement CSP (Communicating
; Sequential Proceses), and channels are used to report
; user interaction events, as well as changing the board
; state.

; ===Channels ===========================================
; the board generates events on this channel
;     {:event :event-symbol
;      :position <int>}
(def board-events (chan))

; the board receives commands to manipulate its state
;     {:command :command-symbol
;      :position <integer>
;      :piece :piece-symbol}

(def board-commands (chan))

; for other processes to acquire the board state atom
;     (atom (create-board))
;; TODO: Set up a go loop to feed that to anyone
;; who asks.
;; It makes a lot more sense than the top level atom that
;; I'm using now
;; Actually, this seems to be the key to this particular
;; design. Have a go-loop in the UI namespace pulling from
;; this channel. Anything that updates state here should
;; sling those updates here instead of just updating the
;; atoms.
;; Or maybe that's just over-complicating things. Maybe
;; this was just a red herring.
(def board-state (chan))

;;; == Board State ==========================================

;; initialize a board, where positions are indexed 1-32.
;; Warning: Comment rot below!
;; each position is an atom containing the symbol of the
;; piece in it.
(defn create-board []
  (atom
   (apply sorted-map
          (flatten
           (map-indexed (fn [i v] (vector (inc i) v))
                        (flatten
                         [(repeat 12 :red-piece)
                          (repeat 8 :empty-piece)
                          (repeat 12 :black-piece)]))))))

;; instantiate our game board state, initializing it
;;  with starting pieces
(def board (create-board))
(def event-stack (atom []))

;;; === Globals ==========================================
;;; Let's be generous and call them "named constants".
;;; These are less evil than board and event-stack above

;; positional constants
(def top-row 1)
(def bottom-row 8)

;; Which pieces belong to any given player?
(def black-pieces #{:black-piece :prom-black-piece})
(def red-pieces #{:red-piece :prom-red-piece})

;;; === Utility Functions =================================

(defn reset []
  (let [blank @(create-board)]
    (swap! board (constantly blank))
    (reset! event-stack [])))

;; given a board position, return the position of neighbors
(defn compute-pos-neighbors
  "Don't use this directly.
Call neighbors to take advantage of memoization instead"
  [pos]
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

;; Memoize neighbors for every board position
;; If this were a really slow process, we'd have some
;; serious UX questions to answer. The most important
;; are:
;; 1. Is this something that's worth delaying the page
;; load to get the cache built?
;; 2. Is it OK to delay any individual interaction
;; to cache a result we haven't already computed?
;; 3. Is the default memoization approach viable?
;; For 32 board positions, we aren't talking about a
;; lot of space.
;; If we had 2^32 or 2^64 spots to cache...we'd probably
;; never want to cache them all.
;; OTOH...for this particular scenario, the "real" solution
;; seems to be using a macro to generate a map at compile
;; time.
;; Real-world considerations are tricky and specific to
;; the actual problem at hand.
(def neighbors (memoize compute-pos-neighbors))
(defn compute-neighbor-positions
  "Don't call this.
We're calling it later in a go block to fill the
cache so calls to neighbors should be practically free."
  []
  (map (fn [pos] {pos (neighbors pos)})
       (range 1 33)))

;;; ==  Event Translators ==================================
;;; An event/request made it to the queue. If the request is legal,
;;; translate it into an appropriate Command so it can be
;;; forwarded along

;;; == Game Rules ==========================================

(defmulti event->command
  "If a synthesized UI event is legal, forward it as a command"
  (fn [{:keys [event]}]
    event))

(defmethod event->command :board-clicked
  [{:keys [position]}]
  (println "Board Clicked event handler")
  ;; TODO: error handling
  (let [content (-> board deref (get position))
        state {:playing-field @board
               :rules @game-state}
        ;; It seems counter-intuitive to deref the game-state
        ;; into rules, then thread through the map on the
        ;; next line to get that same state.
        ;; This is really for the sake of my future self.
        ;; I hope he's as offended as I by all the top-level
        ;; vars we have floating around here.
        my-pieces (if (-> state :rules :blacks-turn?)
                    black-pieces
                    red-pieces)]
    (comment (println "Got through let block. Valid pieces:" my-pieces))
    (if (some my-pieces [content])
      ;; Player clicked on his own piece.
      ;; This means player wants to move it
      {:command :select
       :position position}
      (if (= :empty content)
        (if-let [moving-from (-> state :rules :pending-move)]
          (do
            (println "Moving from" moving-from "to" position "...maybe")
            ;; TODO: Verify that this is a legal switch
            {:command :swap
             :position position
             :from moving-from})
          (println "Nowhere to move from"))
        (println "Blocked by" content)))))

(defn lost?
  "If the board doesn't contain any tiles from this player, he lost the game"
  [board tile-set]
  (when-not (some board tile-set)
    true))

(defn check-for-winner [board]
  ;; TODO: Desperately need to check for scenario where
  ;; current player has no legal moves left
  (cond (lost? board red-pieces) :black
        (lost? board black-pieces) :red))

;;; == Fun! ================================================

(defn victory-dance
  "Someone won.

TODO: Do something fun with the remaining pieces"
  [winner]
  (println "Congratulations" winner))

;;; == Concurrent Processes =================================
;;; From my understanding of Om's architecture, these all really
;;; belong in the baseline IWillMount handler.
;;; Then IWillUnmount should close them.
;;; It seems like semantics, but...this sort of approach is
;;; a recipe for disaster in a big project.

;; this concurrent process reacts to board click events --
(go (while true
      (try
        (let [event (<! board-events)]
          (println "Received synthesized UI event:" event)
          ;; Save these up for playback later
          (swap! event-stack conj event)
          (println "Event history updated")
          (if-let [cmd (event->command event)]
            (do
              (println "POSTing command:" cmd)
              (put! board-commands
                    #_{:command :update-board-position
                       :position (:position event)
                       :piece :black-piece}
                    cmd))
            (println "Illegal Event requested")))
        (catch js/TypeError ex
          (println (pr-str ex "\nWhat's going on here?")))
        (catch :default ex
          ;; It looks like Stack Overflow is wrong, and this
          ;; version of exception handling has not become mainstream
          ;; yet.
          ;; Annoying.
          (println "Unhandled Event Exception:\n" (pr-str ex)))
        (catch js/Object ex
          (println "Seriously Unhandled Exception:\n" (pr-str ex))))))

;; this concurrent process receives board command messages
;; and executes on them.
(go (while true
      (try
        (let [command (<! board-commands)]
          (println "Incoming command:" command)
          (throw (ex-info "Not Implemented" {:problem "This is now broken"}))
          (swap! board assoc (:position command)
                 (:piece command))
          (when-let [winner (check-for-winner @board)]
            (victory-dance winner)
            (reset)))
        (catch js/TypeError ex
          (println (pr-str ex "\nType Error...this is fun!")))
        (catch :default ex
          (println "Unhandled Command Exception\n" (pr-str ex)))
        (catch js/Object ex
          (println "Seriously Unhandled Command Exception:\n" (pr-str ex))))))

;; As a compromise (see comments around the function we're calling),
;; just memoize the neighboring squares in the
;; background.
;; TODO: Stuff this into a defonce so I don't get distracted by the
;; output every time I save.
(go
  ;; Don't want the lazy seq to be discarded without actually doing anything
  ;; This is a cheesy way to handle that
  (let [neighbors (compute-neighbor-positions)]
    (println neighbors "\nmemoized")))
