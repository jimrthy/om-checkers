(ns lg-checkers.core
   (:require [lg-checkers.system :as system]
             [figwheel.client :as fw]))

(defn reset []
  ;; This is what kicks off the magic
  ;; It doesn't seem to happen the first time through, though
  (print "Starting the system")
  ;; Q: Do I really want to restart the entire system every time
  ;; I save?
  (let [state (system/ctor)]
    (print "System started successfully")
    state))

"Top level globals are bad, but trying to debug
without some sort of access to this is a nightmare"
(defonce state
  (atom (reset)))

(enable-console-print!)
(fw/start {:on-jsload (fn [] (println "Q: Do I want to do anything here?"))})


