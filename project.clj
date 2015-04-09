(defproject lg-checkers "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2843"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [om "0.7.1"]
                 [prismatic/schema "0.4.0"]
                 [figwheel "0.2.5"]]

  :plugins [[lein-cljsbuild "1.0.4"]
            [lein-figwheel "0.2.5"]]

  :profiles {:dev {:source-paths ["dev"]}}

  :source-paths ["src"]

  :figwheel {:nrepl-port 7888}

  :cljsbuild {
    :builds [{:id "lg-checkers"
              :source-paths ["src"]
              :compiler {
                :output-to "resources/public/js/lg_checkers.js"
                :output-dir "resources/public/js"
                :optimizations :none
                :source-map true}}]})
