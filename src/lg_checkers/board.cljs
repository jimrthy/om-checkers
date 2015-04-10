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
;; TODO: Add entries for selected piece and legal moves
;; so we can highlight a player's options
(def pieces (s/enum :empty
                    :black-piece
                    :prom-black-piece
                    :red-piece
                    :prom-red-piece))
(def black-pieces (s/enum :black-piece :prom-black-piece))
(def red-pieces (s/enum :red-piece :prom-red-piece))

(def square-description {:row s/Int
                         :column s/Int
                         ;; This is a vector so it can be used as a
                         ;; cursor
                         :content [(s/one pieces "content")]})

;;; The ui namespace is responsible for converting DOM
;;; (real or simulated) events into request-events
(def input-events (s/enum :board-clicked))
(def position {:row s/Int
               :column s/Int})
(def request-event (merge position {:event input-events}))

;;; It's our responsibility to translate those into
;;; command-events and cope with them (legal or not)
(def legal-commands (s/enum :select :swap))
(def command-event {:command legal-commands
                    s/Any s/Any})
(def swap-command (merge command-event {:from position
                                        :to position
                                        :command (s/one :swap "Swap")}))
(def select-command (merge command-event {:at square-description
                                          :command (s/one :select "Select")}))

;;; The board is laid out as a 2-d vector<vector<pieces> >
;;; It is stored in an atom, and bound
;;; to the UI.  Any update of the atom will cause an UI
;;; refresh to reflect the current board state.
;;; We're also storing game state under the poorly chosen
;;; :rules key. TODO: Rename it
;;; :state-stack stores the sequence of states we have gone
;;; through so they can be played back later.
(def board-repr {:playing-field [[pieces]]
                 :rules {:blacks-turn? s/Bool
                         :pending-move square-description}
                 :state-stack []})

(defmulti event->command
  "If a synthesized UI event is legal, forward it as a command"
  (fn [game-state {:keys [event]}]
    event))

(defmulti handle-command
  "Return the appropriate new state of the board after applying command
i.e. Do the actual work"
  (fn [board command]
    (:command command)))

;;; == Internals ==============================================

; === Utility Functions =================================

; positional constants
(def top-row 1)
(def bottom-row 8)

(s/defn get-piece-at :- pieces
  [state :- board-repr
   at :- position]
  (-> state (get (:row at)) (get (:column at))))

; == Board State ==========================================

(s/defn game-over? :- s/Bool
  [game-state]
  (let [contents (-> game-state :playing-field vals)]
    ;; TODO: Also have to check that the current player has
    ;; a legal move available
    (and (some #(or (= :black-piece %)
                    (= :prom-black-piece %)) contents)
         (some #(or (= :red-piece %) (= :prom-red-piece %)) contents))))

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

;;; ==  Event Translators ==================================

;;; I'd like to specify the exact return values for
;;; the next 2 (select-command and swap-command
;;; respectively), but I'm getting errors about
;;; missing Schema.walker protocols for [object Object]
;;; TODO: Figure out why
(s/defn build-select-command :- command-event
  [square :- square-description]
  {:command :select
   :square square})

(s/defn build-swap-command :- command-event
  [from :- square-description
   to :- square-description]
  (println "Moving from (" (:column from)
           "," (:row from) ") to ("
           (:column to) "," (:row to) ")...maybe")
  ;; TODO: Verify that this is a legal switch
  {:command :swap
   :from from
   :to to})

;;; == Public ================================================

;;; An event/request made it to the queue. If the request is legal,
;;; translate it into an appropriate Command so it can be
;;; forwarded along

;; Since this is really the only event that matters,
;; it's very tempting to just put it under the Game Rules
;; section.
;; Actually, it shouldn't even be a multimethod.
;; Oh well.
(s/defmethod event->command
  :board-clicked
   :- (merge command-event
             {s/Any s/Any})
  [state :- board-repr
   {:keys [square] :as ev} :- {:square square-description
                               s/Any s/Any}]
  ;; Will potentially return things like
  #_{:command :update-board-position
     :position (:position event)
     :piece :black-piece}

  ;; piece should actually be a cursor that om can transact!
  ;; on so we don't have to re-render the board every time.
  ;; Mixing concerns of the view down into the business logic
  ;; here seems like a code smell...but the alternatives I've
  ;; been trying don't work correctly.
  (println "Board Clicked event handler:" ev)
  ;; TODO: error handling
  (let [row (:row square)
        column (:column square)
        expected-type (:content square)
        content (-> state :playing-field (get row) (get column))
        _ (assert (= expected-type content))
        my-pieces (if (-> state :rules :blacks-turn?)
                    black-pieces
                    red-pieces)]
    (if (some my-pieces [content])
      ;; Player clicked on his own piece.
      ;; This means player wants to move it
      (build-select-command square)
      (if (= :empty content)
        (if-let [piece-to-move (-> state :rules :pending-move)]
          (build-swap-command piece-to-move square)
          (println "Nowhere to move from"))
        (println "Blocked by" content)))))

;;; == Command Processors =================================
;;; The things that actually do the work

(s/defn clean-slate :- board-repr
  "Set up the beginning of the game"
  []
  {:playing-field (mapv #(mapv vector %)
                        (partition 4 (concat (repeat 12 [:red-piece])
                                             (repeat 8 [:empty-piece])
                                             (repeat 12 [:black-piece]))))
   
   :blacks-turn? true

   ;; When a player clicks on his piece, it's the first half
   ;; of moving it. Keep track of which piece they've told
   ;; us they want to move here
   :piece-to-move nil})

(s/defmethod handle-command :swap :- board-repr
  [state :- board-repr
   {:keys [from to]} #_[:- swap-command]]
  (let [kind (get-piece-at state from)]
    (println "Moving" kind "from" from "to" to)
    ;;; This version seems to do what I want, but I'm not actually
    ;;; seeing any changes. Which is why I started down the rabbit
    ;;; trail of changing everything to components in the UI in the
    ;;; first place.
    (let [result (-> state
                     (update-in [:rules :blacks-turn?] not)
                     (assoc-in [:rules :pending-move] nil)
                     (assoc-in [:playing-field (:row from) (:column from)] :empty)
                     (assoc-in [:playing-field (:row to) (:column to)] kind))]
      (if-let [winner (game-over? result)]
        (do (println "Congratulations" winner)
            ;; TODO: Add a fancy animation to congratulate the winner
            (clean-slate))
        result))))

(s/defmethod handle-command :select :- board-repr
  [state :- board-repr
   {:keys [at]}]
  ;; This is really why game state needs to be in the same atom
  ;; as the playing field. I need to update them both so I can
  ;; add a visual indicator about what was just clicked, and
  ;; refs aren't an option.
  ;; TODO: Get that working.
  (println at "selected")
  (assoc-in state [:rules :pending-move] at))

(defmethod handle-command :default
  [command]
  (println "Unhandled command:\n" command))

