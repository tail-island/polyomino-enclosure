(ns polyomino-enclosure.engine
  (:require [clojure.string :as string]))

(defn- cartesian-product  ; ClojureScriptではmath.combinatoricsが使えなかったので、自前で用意しました。
  [xs ys]
  (for [x xs, y ys]
    [x y]))

(defn create-polyominos
  [definition-strings]
  (letfn [(create-polyomino [definition-string]
            (if-not (empty? definition-string)
              (->> (string/split definition-string #",")
                   (map string/trim)
                   (map #(#+clj Integer/parseInt #+cljs js/parseInt %))
                   (partition 2))
              []))]
    (->> definition-strings
         (map #(create-polyomino (string/trim %))))))

(defn- rotate-clockwise
  [[x y]]
  [(- 0 y) x])

(defn- rotate-counter-clockwise
  [[x y]]
  [y (- 0 x)])

(defn- flip-horizontal
  [[x y]]
  [(- 0 x) y])

(defn- flip-vertical
  [[x y]]
  [x (- 0 y)])

(defn- translate
  [[x y] dx dy]
  [(+ x dx) (+ y dy)])

(defn- operate-polyomino
  [polyomino operation-fn & operation-args]
  (map #(apply operation-fn % operation-args) polyomino))

(defn- xs-and-ys
  [points]
  ((juxt (partial map first)
         (partial map second))
   points))

(defn- board
  [& polyominos]
  (let [points (apply concat polyominos)
        board  (let [[[l t] [r b]] (->> (xs-and-ys points)
                                        ((juxt (partial map #(dec (apply min %)))  ; 番人を使いたいので、一回り大きくしておきます。
                                               (partial map #(inc (apply max %))))))]
                 (zipmap (cartesian-product (take-while #(<= % r) (iterate inc l))
                                            (take-while #(<= % b) (iterate inc t)))
                         (repeat 0)))]
    (reduce #(update-in %1 [%2] inc)
            board
            points)))

(defn- area
  [board & {:keys [search-8-way?]}]
  (letfn [(next-area-point? [point]
            (if-let [value (get board point)]
              (= value (get board [0 0]))
              (throw (ex-info "not enclosed" {}))))
          (next-area-points [[x y]]
            (filter next-area-point? (if-not search-8-way?
                                       [[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]]
                                       (->> (cartesian-product (take-while #(<= % (inc x)) (iterate inc (dec x)))
                                                               (take-while #(<= % (inc y)) (iterate inc (dec y))))
                                            (remove #(= % [x y]))))))
          (area-points [points point-hashes]
            (if-let [point (peek points)]
              (cons point (lazy-seq (apply area-points (reduce (fn [[points point-hashes :as result] next-point]
                                                                 (let [next-point-hash (hash next-point)]
                                                                   (if-not (contains? point-hashes next-point-hash)
                                                                     [(conj points next-point) (conj point-hashes next-point-hash)]
                                                                     result)))
                                                               [(pop points) point-hashes]
                                                               (next-area-points point)))))))]
    (count (area-points (conj #+clj clojure.lang.PersistentQueue/EMPTY #+cljs (.EMPTY cljs.core.PersistentQueue) [0 0]) #{(hash [0 0])}))))

(defn validate-polyominos
  [polyominos]
  (letfn [(validate-form []
            (let [[omino-count & omino-counts] (map count polyominos)]
              (if-not (and (every? #(= % omino-count) omino-counts)
                           (every? #(= (area (board %)) omino-count) polyominos))
                "不正なポリオミノがあります。")))
          (validate-duplication []
            (letfn [(variations [polyomino]
                      (letfn [(rotate-variations [polyomino]
                                (take 4 (iterate #(operate-polyomino % rotate-clockwise) polyomino)))
                              (normalize-polyomino [polyomino]
                                (let [[min-x min-y] (map (partial apply min) (xs-and-ys polyomino))]
                                  (->> polyomino
                                       (map (fn [[x y]] [(- x min-x) (- y min-y)]))
                                       (sort))))]
                        (->> (concat (rotate-variations polyomino)
                                     (rotate-variations (operate-polyomino polyomino flip-horizontal))
                                     (rotate-variations (operate-polyomino polyomino flip-vertical)))
                             (map normalize-polyomino)
                             (distinct))))
                    (validate-polyominos [errors [[polyomino-variations line-number] & more]]
                      (if polyomino-variations
                        (letfn [(validate-polyomino [errors [[other-polyomino-variations other-line-number] & more]]
                                  (if other-polyomino-variations
                                    (recur (if (not-empty (filter (partial apply =) (cartesian-product polyomino-variations other-polyomino-variations)))
                                             (conj errors (str line-number "行目と" other-line-number "行目のポリオミノは重複しています。"))
                                             errors)
                                           more)
                                    errors))]
                          (recur (validate-polyomino errors more)
                                 more))
                        errors))]
              (not-empty (string/join "\n" (validate-polyominos [] (map #(vector (variations %1) %2) polyominos (iterate inc 1)))))))]
    (or (validate-form)
        (validate-duplication))))

(defn create-programs
  [definition-strings]
  (letfn [(create-args [definition-string]
            (if-not (empty? definition-string)
              (->> (string/split definition-string #",")
                   (map string/trim)
                   (map #(#+clj Integer/parseInt #+cljs js/parseInt %)))
              []))
          (create-command [definition-string]
            (if-not (empty? definition-string)
              (->> (next (re-find #"([a-z_]+).*\((.*)\)" definition-string))
                   ((fn [[command-string args-string]] (cons (keyword (string/replace (string/trim command-string) \_ \-))
                                                             (create-args (string/trim args-string))))))
              []))
          (create-program [definition-string]
            (if-not (empty? definition-string)
              (->> (string/split definition-string #";")
                   (map string/trim)
                   (map create-command))
              []))]
    (->> definition-strings
         (map #(create-program (string/trim %))))))
  
(defn execute-programs
  [polyominos programs]
  (letfn [(execute-commands [polyomino [[command & args] & more]]
            (cons polyomino (lazy-seq (if command
                                        (execute-commands (apply operate-polyomino
                                                            polyomino
                                                            (case command
                                                              :rotate-clockwise         rotate-clockwise
                                                              :rotate-counter-clockwise rotate-counter-clockwise
                                                              :flip-horizontal          flip-horizontal
                                                              :flip-vertical            flip-vertical
                                                              :translate                translate)
                                                            args)
                                                          more)))))
          (execute-program [polyomino program]
            (if-not (empty? program)
              (execute-commands polyomino program)
              []))]
    (map execute-program polyominos programs)))

(defn result
  [execute-program-result]
  (map last execute-program-result))

(defn- overlap?
  [board]
  (not (every? #(<= % 1) (vals board))))

(defn- not-enclosed?
  [board]
  (try
    (and (area board :search-8-way? true) nil)
    (catch #+clj clojure.lang.ExceptionInfo #+cljs cljs.core.ExceptionInfo _ true)))

(defn validate-result
  [result]
  (let [board (apply board result)]
    (letfn [(validate-overlaps []
              (if (overlap? board)
                "ポリオミノが重なっています。"))
            (validate-enclosed []
              (if (not-enclosed? board)
                "ポリオミノで囲み込めていません。"))]
      (or (validate-overlaps)
          (validate-enclosed)))))

(defn score
  [result]
  (area (apply board result)))
