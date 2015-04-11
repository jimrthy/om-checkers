(ns lg-checkers.ui
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! >!] :as async]
            [lg-checkers.board :as board]
            [schema.core :as s :refer-macros [defn]]))

(enable-console-print!)

;;; Schema ===============================================

(def async-channel (type (chan)))

(def square-description {:row s/Int
                         :column s/Int
                         :content board/pieces})

;;; == Internals ==========================================

; == UI events ==========================================
; when we click a game square, we send an event

(s/defn board-click
  [channel :- async-channel
   square :- square-description]
  (go (>! channel {:event :board-clicked
                   :square square})))

;;; == Concurrent Processes =================================

(s/defn board-command-loop
  "this concurrent process receives board command messages
   and executes on them.  at present, the only thing it does
   is sets the desired game position to the desired piece"
  [board-commands :- async-channel
   board-atom]
  (println "Entering Board Command loop")
  (go (loop [command (<! board-commands)]
        (when command
          (try
            (print "Command event loop: " command)
            (om/transact! board-atom
                          (fn [old]
                            (let [next (board/handle-command command old)]
                              ;; This isn't as wasteful as it might seem at
                              ;; first glance because of shared immutable state
                              (update-in next [:event-stack] conj next))))
            (catch js/Error ex
              (println ex "\nUnhandled exception thrown"))
            (catch :default ex
              ;; Protect the loop
              (println ex "\nEscaped Command handler")))
          (recur (<! board-commands))))
      (println "Board Commands loop exited")))

(s/defn board-state-source-loop
  "Make the board-state atom more generally available.
Not very useful at the moment, but it seems like a wise
step for future improvements.

It also smells of encapsulation: if I'm making this channel
publicly available, why not just share the board-atom and
bypass this in general?

Then again, maybe transducers can do the deref between
the put and get.

TODO: Research that."
  [board-state :- async-channel
   board-atom]
  (go
    ;; Put returns true unless the channel's closed
    (println "Start the state-pushing loop")
    (loop [still-going (>! board-state board-atom)]
      (try
        (println "Something requested board-state")
        (when still-going
          (recur (>! board-state board-atom)))
        (catch js/Error ex
          (println ex "Unhandled exception thrown"))
        (catch :deault ex
          ;; This doesn't seem to work, but it's
          ;; supposed to be the
          ;; official catch-all handler
          (println ex "Non-exception thrown"))))
    (println "Channel for posting Board State closed")))

;;; == Board UI Drawing ===================================

(s/defn draw-square
  [{:keys [color content row column] :as square}
   owner
   {:keys [board-events] :as opts}]
  (om/component
   (let [base-attrs {:className color}
         clj-attrs (if (= color "white")
                     base-attrs
                     (assoc base-attrs :onClick (fn [e]
                                                  (println "You clicked on a " content)
                                                  (board-click board-events (dissoc square :color)))))
         attrs (clj->js clj-attrs)]
     (if (= color "white")
       (dom/td attrs nil)
       (dom/td attrs (apply dom/div #js{:className (name (ffirst content))} nil))))))

(s/defn draw-pair [square :- square-description
                   owner
                   opts]
  (om/component
   (let [row-n (:row square)
         pair (if (odd? row-n)
                ["white" "green"]
                ["green" "white"])
         squares (map
                  #(assoc square :color %)
                  pair)]
     (apply dom/span nil (om/build-all
                          draw-square
                          squares
                          {:opts opts})))))

(defn draw-row [row owner opts]
  (om/component
   (let [n (:row row)
         content (:content row)]
     (apply dom/tr nil
            (om/build-all
             draw-pair
             (map-indexed
              (fn [column cell]
                {:content cell, :row n, :column column})
              (:content row))
             {:opts  opts})))))

(defn checkerboard
  "given a checkerboard data structure, partition into
rows and draw the individual rows"
  [board owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (println "Mounting Checkerboard")
      (let [
            ;; the board generates events on this channel
            ;;     {:event :event-symbol
            ;;      :position <int>}
            board-events (chan)

            ;; the board receives commands to manipulate its state
            ;;     {:command :command-symbol
            ;;      :position <integer>
            ;;      :piece :piece-symbol}
            board-commands (chan)

            ;; for other processes to acquire the board state atom
            ;;     (atom (create-board))
            board-state (chan)]
        (comment (board-event-loop board-events board board-commands))
        (board-command-loop board-commands board)
        (board-state-source-loop board-state board)

        ;; Child components need access to them
        (println "Async channels created. Assigning to component local state")
        (om/set-state! owner :board-state board-state)
        (om/set-state! owner :board-events board-events)
        (om/set-state! owner :board-commands board-commands)
        (println "UI State set")))

    om/IRenderState
    (render-state [this {:keys [board-events] :as state}]
      (let [board-events-debugging-garbage (chan)]
        (println "Rendering the playing field:\n" (pr-str board))
        (dom/table nil
                   (apply dom/tbody nil
                          ;; aka (->> board :playing-field (partition 4) (om/build-all draw-row))
                          ;; I'm honestly torn about which version is more readable,
                          ;; but this one's probably more idiomatic for Om
                          (om/build-all draw-row (map-indexed (fn [i v]
                                                                {:row i
                                                                 :content v})
                                                              (:playing-field board))
                                        {:opts {:board-events board-events}})))))
    om/IWillUnmount
    (will-unmount [this]
      (println "Unmounting the checkerboard")
      (doseq [channel-name [:board-state :board-events :board-commands]]
        (let [channel (om/get-state owner channel-name)]
          (async/close! channel))))))

; == Bootstrap ============================================
(defn bootstrap-ui [board]
  (assert board "Missing board from system")
  (println "UI - Binding board to root")
  (let [root (om/root
              checkerboard       ; top of Component chain
              board              ; our game state
              {:target (. js/document (getElementById "checkers"))
               :shared {}})]
    (println "UI - checkers renderer bound")
    root))
