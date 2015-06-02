(ns polyomino-enclosure.core
  (:use     (polyomino-enclosure engine))
  (:require (clojure             [string :as string])
            (clojure.java        [shell  :as shell]))
  (:gen-class))

(defn- println-err
  [& more]
  (binding [*out* *err*]
    (apply println more)))

(defn -main
  [question-file solution-program]
  (let [question        (slurp question-file)
        start-nano-time (System/nanoTime)
        answer          (:out (shell/sh solution-program :in question))
        end-nano-time   (System/nanoTime)
        polyominos      (create-polyominos (string/split-lines question))
        programs        (create-programs   (string/split-lines answer))]
    (if-let [error (validate-game polyominos programs)]
      (println-err error)
      (let [result (result (execute-programs polyominos programs))]
        (if-let [error (validate-result result)]
          (println-err error)
          (println (/ (double (- end-nano-time start-nano-time)) 1000000.0)
                   (score result))))))
  (shutdown-agents))
