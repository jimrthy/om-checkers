(ns lg-checkers.core
   (:require [lg-checkers.system :as system]
             [figwheel.client :as fw]))

(enable-console-print!)
(fw/start {:on-jsload (fn []
                        ;; This is what kicks off the magic
                        (print "Starting the system")
                        (system/ctor)
                        (print "System started successfully"))})


