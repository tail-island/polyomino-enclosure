(ns polyomino-enclosure.core
  (:use     (polyomino-enclosure engine))
  (:require (clojure             [string :as string]))
  (:gen-class))

(defn- println-err
  [& more]
  (binding [*out* *err*]
    (apply println more)))

(defn -main
  [question-file answer-file]
  (let [question-string (slurp question-file)
        answer-string   (slurp answer-file)]
    (if-let [error (validate-question question-string)]
      (do (println-err error)
          (println "0"))
      (let [polyominos (create-polyominos (string/split-lines question-string))
            programs   (create-programs   (string/split-lines answer-string))]
        (if-let [error (or (validate-game polyominos programs)
                           (validate-result (result (execute-programs polyominos programs))))]
          (do (println-err error)
              (println "0"))
          (println (score (result (execute-programs polyominos programs)))))))))
