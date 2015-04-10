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

;;; == Internals ==========================================

; == UI events ==========================================
; when we click a game square, we send an event


;;; == Concurrent Processes =================================

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

; given a checkerboard data structure, partition into
; rows and draw the individual rows
(defn checkerboard [board owner]
  (reify
    om/IRender
    (render [this]
      (dom/p "Stop the insanity"))
))

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
