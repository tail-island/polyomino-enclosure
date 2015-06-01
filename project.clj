(defproject polyomino-enclosure "0.1.0-SNAPSHOT"
  :description  "polyomino-enclosure: A test environment for NUL programming coucours 2015 problem #2."
  :url          "https://github.com/tail-island/polyomino-enclosure"
  :license      {:name "Eclipse Public License"
                 :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure       "1.7.0-RC1"]
                 [org.clojure/clojurescript "0.0-3269"]
                 [org.clojure/core.async    "0.1.346.0-17112a-alpha"]
                 [org.omcljs/om             "0.8.8"]
                 [sablono                   "0.3.4"]]
  :source-paths ["src" "target/classes"]
  :plugins      [[com.keminglabs/cljx "0.6.0"]
                 [lein-cljsbuild      "1.0.6"]]
  :cljx         {:builds [{:source-paths ["src-cljx"]
                           :output-path  "target/classes"
                           :rules        :clj}
                          {:source-paths ["src-cljx"]
                           :output-path  "target/classes"
                           :rules        :cljs}]}
  :cljsbuild    {:builds [{:source-paths ["src-cljs" "target/classes"]
                           :compiler     {:main       polyomino-enclosure.core
                                          :output-dir "resources/public/cljs"
                                          :output-to  "resources/public/cljas/polyomino-enclosure.js"}}]})