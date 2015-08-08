(defproject polyomino-enclosure "0.1.0-SNAPSHOT"
  :description  "polyomino-enclosure: A test environment for NUL programming coucours 2015 problem #2."
  :url          "https://github.com/tail-island/polyomino-enclosure"
  :license      {:name "Eclipse Public License"
                 :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure       "1.7.0"]
                 [org.clojure/clojurescript "1.7.48"]
                 [org.clojure/core.async    "0.1.346.0-17112a-alpha"]
                 [org.omcljs/om             "0.9.0"]
                 [sablono                   "0.3.5"]]
  :source-paths ["src" "target/classes"]
  :main         polyomino-enclosure.core
  :profiles     {:uberjar {:aot :all}}
  :plugins      [[com.keminglabs/cljx "0.6.0"]
                 [lein-cljsbuild      "1.0.6"]]
  :cljx         {:builds [{:source-paths ["src-cljx"]
                           :output-path  "target/classes"
                           :rules        :clj}
                          {:source-paths ["src-cljx"]
                           :output-path  "target/classes"
                           :rules        :cljs}]}
  :cljsbuild    {:builds [{:source-paths ["src-cljs" "target/classes"]
                           :compiler     {:asset-path "cljs"
                                          :main       polyomino-enclosure.gui
                                          :output-dir "resources/public/cljs"
                                          :output-to  "resources/public/cljs/polyomino-enclosure.js"}}]}
  :aliases      {"uberjar" ["do" "clean," "cljx" "once," "uberjar"]}
  :auto-clean   false)
