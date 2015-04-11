(ns lg-checkers.core
   (:require [lg-checkers.system :as system]
             [figwheel.client :as fw]))

(enable-console-print!)
(fw/start {:on-jsload (fn []
                        ;; This is what kicks off the magic
                        ;; It doesn't seem to happen the first time through, though
                        (print "Starting the system")
                        ;; Q: Do I really want to restart the entire system every time
                        ;; I save?
                        (system/ctor)
                        (print "System started successfully"))})


