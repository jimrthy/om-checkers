(ns lg-checkers.ui
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <! >!] :as async]
            [lg-checkers.board :as board]
            [schema.core :as s :refer-macros [defn]]))

(enable-console-print!)

; Schema ===============================================

;; Describes what's at each position on the playing field
(comment (def cell-description [(s/one s/Int "position") (s/one board/pieces "contents")]))

;; Just so we have a handle on the available options
(def legal-colors (s/enum "green" "white"))

(def square-description {:row s/Int
                         :column s/Int
                         ;; This is a vector so it can be used as a
                         ;; cursor
                         :content [(s/one board/pieces "content")]})
(def colored-square (assoc square-description :color legal-colors))

(def row-description {:row s/Int
                      :content board/pieces})

; == Internals ==========================================

(s/defn pick-current-row
  "Which row is this sequence of cells describing?"
  [descr :- [square-description]]
  (throw (ex-info "Obsolete" {:description descr}))
  ;; It's probably a little more efficient to hard-code the width
  ;; But vectors (which is what we should really be passing
  ;; into here, if we care about performance) already know
  ;; their length.
  ;; More importantly, this makes it easier to debug a
  ;; subset.
  (let [width (count descr)]
    (-> descr last first (/ width))))

; == UI events ==========================================
; when we click a game square, we send an event
(defn board-click [event-channel square]
  (println "board-click: forwarding DOM click")
  (put! event-channel {:event :board-clicked
                       :position square}))

; == Board UI Drawing ===================================
; draw pieces based on the piece-type
(s/defn draw-piece
  "Still have to pass in the full map. If we just supply a primitive, we won't get a cursor"
  [square :- colored-square
   owner]
  (println "Top of draw-piece")
  (om/component
   ;; If I convert this to a span, the pieces don't get
   ;; drawn.
   (let [piece-type (:color square)]
     (comment (println "Rendering a" piece-type "at (" (:column square) "," (:row square)))
     (dom/div #js {:className piece-type} nil))))

(s/defn draw-square [square :- colored-square
                     owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [board-events] :as state}]
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

; draws pairs of checkerboard squares within a row
; depending on if row is odd or even.
(s/defn draw-tuple [square :- square-description
                    owner]
  (om/component
   (println "Rendering square pairs in draw-tuple with " square)
   (let [piece-type (name (:content square))
         row-odd? (odd? (:row square))
         pair (if row-odd?
                ["white" "green"]
                ["green" "white"])]
     (apply dom/span nil
            (map #(om/build draw-square
                            (assoc square :color %))
                 pair)))))

; given a row, determine if it is an odd or even row
; and iterates over the board positions, drawing each
; tuple of checkerboard squares
(s/defn draw-row [cells :- row-description
                  owner]
  (om/component
   (apply dom/tr nil
          (om/build-all draw-tuple (map-indexed (fn [i v]
                                                  (assoc v :column (inc i)))
                                                cells)))))

; given a checkerboard data structure, partition into
; rows and draw the individual rows
(defn checkerboard [board owner]
  (reify
    om/IWillMount
    (will-mount [this]
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

        ;; Child components need access to them
        (om/set-state! owner :board-state board-state)
        (om/set-state! owner :board-events board-events)
        (om/set-state! owner :board-commands board-commands)

        ;; == Concurrent Processes =================================

        ;; this concurrent process reacts to board click events --
        ;; at present, it sets the board position clicked to contain
        ;; a black piece by sending a command to the board-commands
        ;; channel
        (go (loop [event (<! board-events)]
              (when event
                (try  ; Protect the loop
                  (println "UI event loop: " event)
                  (when-let [command (board/event->command board event)]
                    (println "Sending command to update the game")
                    (>! board-commands command))
                  (catch :default ex
                    (println ex "\n escaped event translator")))
                (recur (<! board-events))))
            (println "Event loop exiting due to closed channel"))

        ;; this concurrent process receives board command messages
        ;; and executes on them.  at present, the only thing it does
        ;; is sets the desired game position to the desired piece
        (go (loop [command (<! board-commands)]
              (when command
                (try
                  (print "Command event loop: " command)

                  (om/transact! board (partial board/board-update command))
                  (catch :default ex
                    ;; Protect the loop
                    (println ex "\nEscaped Command handler")))
                (recur (<! board-commands))))
            (println "Board Commands loop exited"))

        
        (go
          (loop [still-going (>! board-state board)]
            (when still-going
              (recur (>! board-state board))))
          (println "Channel for posting Board State closed"))))

    om/IRender
    (render [this]
      (println "Rendering the playing field:\n" (pr-str board))
      (dom/table nil
                 (apply dom/tbody nil
                        ;; aka (->> board :playing-field (partition 4) (om/build-all draw-row))
                        ;; I'm honestly torn about which version is more readable,
                        ;; but this one's probably more idiomatic
                        (om/build-all draw-row (map (fn [i v]
                                                      (assoc v :row i))
                                                    (:playing-field board))))))
    om/IWillUnmount
    (will-unmount [this]
      (doseq [channel [:board-events :board-commands :board-state]]
        (when-let [channel (om/get-state :board-events)]
          (async/close! channel))))))

; == Bootstrap ============================================
(defn bootstrap-ui [board]
  (let [
        root (om/root
              checkerboard       ; top of Component chain
              board              ; our game state
              {:target (. js/document (getElementById "checkers"))
               :shared {}})]
    root))

