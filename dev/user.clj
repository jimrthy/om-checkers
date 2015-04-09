(ns user
  "Every clojure program needs dev-time dependencies"
  (:require [figwheel-sidecar.repl-api :as repl-api]))

(defn cljs
  "Switch to the cljs REPL.

N.B. this is predicated on having `lein figwheel` running somewhere
and an NREPL connected to its port"
  []
  (repl-api/cljs-repl))
