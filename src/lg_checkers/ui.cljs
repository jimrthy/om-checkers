(ns lg-checkers.ui
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [lg-checkers.board :refer [board board-events]]))

(enable-console-print!)

; == UI events ==========================================
; when we click a game square, we send an event
(defn board-click [#_board-pos piece]
  (put! board-events {:event :board-clicked
                      #_ (comment :position board-pos)
                      :piece piece}))

; == Board UI Drawing ===================================
; draw pieces based on the piece-type
(defn component-piece [piece-type owner]
  (om/component
   (apply dom/div #js {:className piece-type} nil)))
(defn draw-piece [piece-type]
  (apply dom/div #js {:className piece-type} nil))

; draws pairs of checkerboard squares within a row
; depending on if row is odd or even.
(defn draw-tuple [piece owner {:keys [row-odd?] :as opts}]
  (om/component
   (comment) (println piece "\n" (pr-str opts) "\n" (keys opts))
   (let [piece-type (name (last piece))
         piece-pos (first piece)
         white-square (dom/td #js {:className "white" :id (str "white-" piece-pos)})
         green-square (dom/td #js {:className "green" :id (str "green-" piece-pos)
                                   :onClick
                                   (fn [e]
                                     (println "Clicked on a" piece-type "at" piece-pos)
                                     (board-click #_piece-pos piece))}
                              #_(draw-piece piece-type)
                              (om/build component-piece piece-type))]
     ;; This seems to be where my invalid checksum warning stems from.
     ;; It showed up when I converted this to a Component
     (apply dom/span nil
            (if row-odd?
              [white-square green-square]
              [green-square white-square])))))

(defn draw-tuple-old [piece {:keys [row-odd?] :as opts}]
  (let [piece-type (name (last piece))
        piece-pos (first piece)
        white-square (dom/td #js {:className "white" :id (str "white-" piece-pos)})
        green-square (dom/td #js {:className "green" :id (str "green-" piece-pos)
                                  :onClick
                                  (fn [e]
                                    (println "Clicked on a" piece-type "at" piece-pos)
                                    (board-click
                                     piece-pos))}
                             (draw-piece piece-type))]
    (comment (apply dom/span nil))
    (if row-odd?
      [white-square green-square]
      [green-square white-square])))

; given a row, determine if it is an odd or even row
; and iterates over the board positions, drawing each
; tuple of checkerboard squares
(defn draw-row [row owner]
  (om/component
   (let [curr-row (/ (first (last row)) 4)
         row-odd? (odd? curr-row)]
     (apply dom/tr nil
            (om/build-all draw-tuple row {:opts {:row-odd? row-odd?}})
            #_(mapcat #(draw-tuple-old % {:row-odd? row-odd?}) row)))))

; given a checkerboard data structure, partition into
; rows and draw the individual rows
(defn checkerboard [board owner]
  (om/component
   (apply dom/table nil
      (om/build-all draw-row
           (partition 4 board)))))

; == Bootstrap ============================================
(defn bootstrap-ui []
  (om/root
    checkerboard ; our UI
    board        ; our game state
    {:target (. js/document (getElementById "checkers"))}))

(bootstrap-ui)
