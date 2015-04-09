(ns user
  "Every clojure app is going to need some sort of dev-time helpers"
  (:require [figwheel-sidecar.repl-api :as repl-api]))

(defn cljs
  "Because it gets annoying to google for the process"
  []
  (repl-api/cljs-repl))
