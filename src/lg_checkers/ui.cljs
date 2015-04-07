(ns lg-checkers.ui
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [lg-checkers.board :as board]
            [schema.core :as s :refer-macros [defn]]))

(enable-console-print!)

; Schema ===============================================

;; Describes what's at each position on the playing field
(def cell-description [(s/one s/Int "position") (s/one board/pieces "contents")])

; == Internals ==========================================

(s/defn pick-current-row
  "Which row is this sequence of cells describing?"
  [descr :- [cell-description]]
  (-> descr last first (/ 4)))

; == UI events ==========================================
; when we click a game square, we send an event
(defn board-click [board-pos]
  (println "DOM click")
  (put! board/board-events {:event :board-clicked
                            :position board-pos}))

; == Board UI Drawing ===================================
; draw pieces based on the piece-type
(defn draw-piece [piece-type]
  (om/component
   (dom/div #js {:className piece-type} nil)))

; draws pairs of checkerboard squares within a row
; depending on if row is odd or even.
(s/defn draw-tuple [row-odd? :- s/Bool
                    piece :- [(s/one s/Int "position") (s/one s/Keyword "piece-type")]]
  (om/component
   (let [piece-type (name (last piece))
         piece-pos (first piece)
         white-square (dom/td #js {:className "white"})
         green-square (dom/td #js {:className "green"
                                   :onClick
                                   (fn [e] (board-click
                                            piece-pos))}
                              (om/build draw-piece piece-type))]
     (apply dom/span nil (if row-odd?
                           [white-square green-square]
                           [green-square white-square])))))

; given a row, determine if it is an odd or even row
; and iterates over the board positions, drawing each
; tuple of checkerboard squares
(s/defn draw-row [cells :- [cell-description]]
  (om/component
   (let [curr-row (pick-current-row cells)
         row-odd? (odd? curr-row)]
     (apply dom/tr nil
            (om/build-all (partial draw-tuple row-odd?) cells)))))

; given a checkerboard data structure, partition into
; rows and draw the individual rows
(defn checkerboard [board owner]
  (om/component
   (println "Rendering the playing field:\n" (pr-str board))
   (dom/table nil
              (apply dom/tbody nil
                     (om/build-all draw-row (partition 4 (:playing-field board)))))))

; == Bootstrap ============================================
(defn bootstrap-ui []
  (om/root
    checkerboard ; our UI
    board/board        ; our game state
    {:target (. js/document (getElementById "checkers"))}))

(bootstrap-ui)
