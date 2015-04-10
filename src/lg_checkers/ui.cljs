(ns lg-checkers.ui
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! >!] :as async]
            [lg-checkers.board :as board]
            [schema.core :as s :refer-macros [defn]]))

(enable-console-print!)

;;; Schema ===============================================

;; Just so we have a handle on the available options
(def legal-colors (s/enum "green" "white"))

(def async-channel (type (chan)))
(def row-description {:row s/Int
                      :content board/pieces
                      #_[:board-events async-channel]})

(def colored-square (assoc board/square-description :color legal-colors))

;;; == Internals ==========================================

; == UI events ==========================================
; when we click a game square, we send an event
(s/defn board-click :- s/Bool
  [event-channel
   square :- colored-square]
  (println "board-click: " square)
  ;; It seems wrong to shuffle this into a go
  ;; block.
  ;; I'm not sure where things broke, but every
  ;; alternative I've tried fails.
  ;; Then again, maybe I just need to do something
  ;; like a lein clean or a page refresh.
  ;; (I've tried both of those, but there could
  ;; easily be something along those lines that I'm
  ;; missing)
  (go
    (>! event-channel {:event :board-clicked
                       :square square})))

;;; == Concurrent Processes =================================

(s/defn board-event-loop
  "this concurrent process reacts to board click events"
  [board-events :- async-channel
   board-atom
   board-commands :- async-channel]
  (go (loop [event (<! board-events)]
        (when event
          (try  ; Protect the loop
            (println "UI event loop: " event)
            (when-let [command (board/event->command board-atom event)]
              (println "Legal command for updating the game")
              (>! board-commands command))
            (catch :default ex
              (println ex "\n escaped event translator")))
          (recur (<! board-events))))
      (println "Event loop exiting due to closed channel")))

(s/defn board-command-event-loop
  "this concurrent process receives board command messages
   and executes on them.  at present, the only thing it does
   is sets the desired game position to the desired piece"
  [board-commands :- async-channel
   board-atom]
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
      (println "Something requested board-state")
      (when still-going
        (recur (>! board-state board-atom))))
    (println "Channel for posting Board State closed")))

;;; == Board UI Drawing ===================================

(comment
  (s/defn draw-piece
    "draw pieces based on the piece-type
Still have to pass in an associative. If we just supply a primitive, we won't get a cursor"
    [piece-type :- [(s/one board/pieces "Kind")]
     owner]
    (println "Top of draw-piece")
    (om/component
     ;; If I convert this to a span, the pieces don't get
     ;; drawn.
     (let [piece-type (first piece-type)]
       (comment (println "Rendering a" piece-type))
       (dom/div #js {:className piece-type} nil))))

  (s/defn draw-square [square :- colored-square
                       owner
                       {:keys [board-events] :as opts}]
    (reify
      om/IRender
      (render [_]
        (let [color (:color square)]
          (println "Drawing a" color "square at (" (:column square) "," (:row square) ")")
          (let [attrs (if (= "green" color)
                        (do
                          (comment (println "Have a green square"))
                          #js {:className color
                               :onClick (fn [e]
                                          ;; Note that the event here will be recycled.
                                          ;; If we were to do anything with it, we'd
                                          ;; need to extract whichever data we need or
                                          ;; call its (persist) method before we tried
                                          ;; to put it on a channel.
                                          (println "DOM click on square (" (:column square) "," (:row square) ")")
                                          (board-click board-events square)
                                          (println "Event placed on channel"))})
                        (do
                          (comment (println "Color: " color))
                          #js {:className color}))]
            (println attrs)
            (dom/td attrs
                    (when (= "green" color)
                      (om/build draw-piece (:content square)))))))))

  (s/defn draw-tuple
    "draws pairs of checkerboard squares within a row
depending on if row is odd or even."
    [square :- board/square-description
     owner
     {:keys [board-events] :as opts}]
    (reify
      om/IRender
      (render [_]
        (println "Rendering square pairs in draw-tuple with " square)
        (let [piece-type (name (:content square))
              row-odd? (odd? (:row square))
              pair (if row-odd?
                     ["white" "green"]
                     ["green" "white"])]
          (om/build-all draw-square (map #(assoc square
                                                 :color %)
                                         pair)
                        {:opts {:board-events board-events}})))))

  (s/defn draw-row
    "given a row, determine if it is an odd or even row
and iterates over the board positions, drawing each
tuple of checkerboard squares"
    [cells :- row-description
                    owner
                    {:keys [board-events] :as opts}]
    (om/component
     (apply dom/tr nil
            (om/build-all draw-tuple (map-indexed (fn [i v]
                                                    (assoc v
                                                           :column (inc i)))
                                                  cells)
                          {:opts {:board-events board-events}})))))

; given a checkerboard data structure, partition into
; rows and draw the individual rows
(defn checkerboard [board owner]
  (reify
    #_[om/IWillMount
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
                     (board-event-loop board-events board board-commands)
                     (board-command-event-loop board-commands board)
                     (board-state-source-loop board-state board)

                     ;; Child components need access to them
                     (println "Async channels created. Assigning to component local state")
                     ;; Commenting these out proved that these next few lines aren't the problem
                     (comment)
                     (om/set-state! owner :board-state board-state)
                     (om/set-state! owner :board-events board-events)
                     (om/set-state! owner :board-commands board-commands)
                     (println "UI State set")))]

    #_[om/IRenderState
       (render-state [this {:keys [board-events] :as state}]
                     (let [board-events-debugging-garbage (chan)]
                       (println "Rendering the playing field:\n" (pr-str board))
                       (dom/table nil
                                  (apply dom/tbody nil
                                         ;; aka (->> board :playing-field (partition 4) (om/build-all draw-row))
                                         ;; I'm honestly torn about which version is more readable,
                                         ;; but this one's probably more idiomatic
                                         (om/build-all draw-row (map (fn [i v]
                                                                       (assoc v
                                                                              :row i))
                                                                     (:playing-field board))
                                                       {:opts {:board-events board-events}})))))]
    om/IRender
    (render [this]
      (dom/p "Stop the insanity"))
    #_[om/IWillUnmount
       (will-unmount [this]
                     (println "Unmounting the checkerboard (in theory)")
                     (doseq [channel [:board-events :board-commands :board-state]]
                       (comment (when-let [channel (om/get-state owner channel)]
                                  (println "Closing" channel)
                                  (async/close! channel)))))]))

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
