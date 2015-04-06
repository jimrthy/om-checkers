(ns lg-checkers.core
   (:require [lg-checkers.ui :refer [bootstrap-ui]]
             [figwheel.client :as fw]))

(enable-console-print!)
(fw/start {:on-jsload (fn []
                        (print "reloaded"))})
(bootstrap-ui)
