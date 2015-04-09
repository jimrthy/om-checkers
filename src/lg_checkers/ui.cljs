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
(defn board-click [board-pos]
  (println "board-click: forwarding DOM click")
  (put! board/board-events {:event :board-clicked
                            :position board-pos}))

; == Board UI Drawing ===================================
; draw pieces based on the piece-type
(defn draw-piece [_ owner
                  {:keys [piece-pos piece-type] :as opts}]
  (println "Top of draw-piece")
  (om/component
   ;; If I convert this to a span, the pieces don't get
   ;; drawn.
   (comment (println "Rendering a" piece-type "at " piece-pos))
   (dom/div #js {:className piece-type :id (str piece-type "-" piece-pos)} nil)))

(s/defn draw-square [_
                     owner
                     {:keys [color piece-pos piece-type] :as opts} :- {:color (s/enum "white" "green")
                                                                       :piece-pos s/Int
                                                                       :piece-type s/Str}]
  (om/component
   (println "Drawing the square at" piece-pos)
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
                                   (println "DOM click on square " piece-pos)
                                   (board-click piece-pos)
                                   (println "Event placed on channel"))})
                 (do
                   (comment (println "Color: " color))
                   #js {:className color}))]
     (println attrs)
     (dom/td attrs
             (when (= "green" color)
               (om/build draw-piece nil {:opts {:piece-type piece-type
                                                :piece-pos piece-pos}}))))))

; draws pairs of checkerboard squares within a row
; depending on if row is odd or even.
(s/defn draw-tuple [piece :- [(s/one s/Int "position") (s/one s/Keyword "piece-type")]
                    owner
                    {:keys [row-odd?] :as opts} :- {:row-odd? s/Bool}]
  (om/component
   (println "Rendering square pairs in draw-tuple with " piece)
   (let [piece-type (name (last piece))
         piece-pos (first piece)
         pair (if row-odd?
                ["white" "green"]
                ["green" "white"])]
     (apply dom/span nil
            (map #(om/build draw-square
                            nil 
                            {:opts {:color % :piece-pos piece-pos, :piece-type piece-type}})
                 pair)))))

; given a row, determine if it is an odd or even row
; and iterates over the board positions, drawing each
; tuple of checkerboard squares
(s/defn draw-row [cells :- [[cell-description]]
                  owner]
  (om/component
   (let [curr-row (pick-current-row cells)
         row-odd? (odd? curr-row)]
     (println "Drawing row" curr-row)
     (apply dom/tr nil
            (om/build-all draw-tuple cells {:opts {:row-odd? row-odd?}})))))

; given a checkerboard data structure, partition into
; rows and draw the individual rows
(defn checkerboard [board owner]
  (om/component
   (println "Rendering the playing field:\n" (pr-str board))
   (dom/table nil
              (apply dom/tbody nil
                     ;; aka (->> board :playing-field (partition 4) (om/build-all draw-row))
                     ;; I'm honestly torn about which version is more readable,
                     ;; but this one's probably more idiomatic
                     (om/build-all draw-row (:playing-field board))))))

; == Bootstrap ============================================
(defn bootstrap-ui []
  (om/root
    checkerboard       ; top of Component chain
    board/board        ; our game state
    {:target (. js/document (getElementById "checkers"))}))

(bootstrap-ui)
