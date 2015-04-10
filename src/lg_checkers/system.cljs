(ns lg-checkers.system
  "Tie the pieces and dependencies together"
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [lg-checkers.board :as board]
            [lg-checkers.ui :as ui]))

(enable-console-print!)

(def reset nil)

(defn ctor
  []
  (let [game-state (atom (board/clean-slate))
        om-root (ui/bootstrap-ui game-state)]
    ;; Make sure the memoization happens
    ;; Q:  Is it worth delaying startup to get these
    ;; cached?
    ;; A: For something this small and simple, it could
    ;; go either way.
    ;; For a real scenario, it would probably be wise
    ;; to have the server compute it.
    ;; For that matter, it might be best to do something
    ;; like this as a macro that expands to a
    ;; hashmap that we can look up directly.
    ;; For now, just run this in a background thread
    (go
      ;; print it to avoid the lazy sequence from being
      ;; discarded
      ;; TODO: Do this inside something like a defonce
      ;; instead so I don't have to look at it every time
      ;; through the loop.
      ;; TODO: Research using anync/thread in clojurescript.
      (println (board/compute-neighbor-positions)))))
