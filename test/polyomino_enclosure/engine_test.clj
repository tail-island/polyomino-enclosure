(ns polyomino-enclosure.engine-test
  (:use     (polyomino-enclosure engine))
  (:require (clojure             [test   :refer :all]
                                 [pprint :refer :all]))
  (:import  (clojure.lang        ExceptionInfo)))

(def ^:private polyomino-definition-strings
  ["0,0,0,1,0,2,0,3,1,3"
   "0,0,0,1,-1,1,-1,2,-1,3"
   "0,0,0,1,1,1,2,1,2,0"
   "0,0,0,1,0,2,1,2,2,2"
   "0,0,0,1,1,1,1,2,2,2"
   "0,0,1,0,1,1,1,2,2,2"
   "0,0,1,0,2,0,3,0,4,0"])

(def ^:private program-definition-strings
  ["rotate_counter_clockwise();translate(0,1);"
   "flip_vertical();translate(-4,3);"
   ""
   "flip_vertical();translate(-5,-1);"
   "flip_horizontal();translate(-1,1);"
   "translate(1,-3);rotate_clockwise();rotate_clockwise();rotate_clockwise();rotate_clockwise();"
   "translate(-3,-4);"])

(deftest test-validate-question
  (is (= "問題の2行目、空行です。問題の4行目、空行です。"
         (validate-question "0,0,0,1,0,2,0,3,1,3\n\n0,0,0,1,-1,1,-1,2,-1,3\n\n0,0,0,1,1,1,2,1,2,0")))
  (is (= nil  ; 最後の空行は無視。
         (validate-question "0,0,0,1,0,2,0,3,1,3\n0,0,0,1,-1,1,-1,2,-1,3\n0,0,0,1,1,1,2,1,2,0\n")))
  (is (= "問題の2行目、不正な文字が含まれています。問題の3行目、不正な文字が含まれています。"
         (validate-question "0,0,0,1,0,2,0,3,1,3\n0,0,0,1,-1,1,-1,2,-1, 3\n0,0,0,1,1,1,2,1,2,0 ")))
  (is (= "問題の2行目、行の最後の文字が数字ではありません。"
         (validate-question "0,0,0,1,0,2,0,3,1,3\n0,0,0,1,-1,1,-1,2,-1,3,\n0,0,0,1,1,1,2,1,2,0")))
  (is (= "問題の2行目、数値の個数が奇数です。"
         (validate-question "0,0,0,1,0,2,0,3,1,3\n0,0,0,1,-1,1,-1,2,-1,3,1\n0,0,0,1,1,1,2,1,2,0")))
  (is (= "問題の2行目、不正なフォーマットの数値が含まれています。問題の3行目、不正なフォーマットの数値が含まれています。"
         (validate-question "0,0,0,1,0,2,0,3,1,3\n0,0,0,1-,-1,1,-1,2,-1,3\n0,0,0,1,1,1,2,1,02,0"))))

(deftest test-create-polyominos
  (is (= [[[ 0  0] [ 0  1] [ 0  2] [ 0  3] [ 1  3]]
          [[ 0  0] [ 0  1] [-1  1] [-1  2] [-1  3]]
          [[ 0  0] [ 0  1] [ 1  1] [ 2  1] [ 2  0]]
          [[ 0  0] [ 0  1] [ 0  2] [ 1  2] [ 2  2]]
          [[ 0  0] [ 0  1] [ 1  1] [ 1  2] [ 2  2]]
          [[ 0  0] [ 1  0] [ 1  1] [ 1  2] [ 2  2]]
          [[ 0  0] [ 1  0] [ 2  0] [ 3  0] [ 4  0]]]
         (create-polyominos polyomino-definition-strings))))

(deftest test-board
  (is (= {[0 0] 0, [1 0] 0, [2 0] 0, [3 0] 0,
          [0 1] 0, [1 1] 2, [2 1] 1, [3 1] 0,
          [0 2] 0, [1 2] 1, [2 2] 0, [3 2] 0,
          [0 3] 0, [1 3] 0, [2 3] 0, [3 3] 0}
         (apply #'polyomino-enclosure.engine/board ((juxt #(-> %
                                                               (#'polyomino-enclosure.engine/operate-polyomino #'polyomino-enclosure.engine/rotate-clockwise)
                                                               (#'polyomino-enclosure.engine/operate-polyomino #'polyomino-enclosure.engine/translate 1 1))
                                                          #(-> %
                                                               (#'polyomino-enclosure.engine/operate-polyomino #'polyomino-enclosure.engine/translate 1 1)))
                                                    [[0 0] [1 0]])))))

(deftest test-area-size
  (is (= 3 (#'polyomino-enclosure.engine/area-size (#'polyomino-enclosure.engine/board [[0 0] [1 0] [2 0]]))))
  (is (= 2 (#'polyomino-enclosure.engine/area-size (#'polyomino-enclosure.engine/board [[0 0] [1 0] [2 1]]))))
  (is (= 3 (#'polyomino-enclosure.engine/area-size (#'polyomino-enclosure.engine/board [[0 0] [1 0] [2 1]])
                                                   :search-8-way? true)))
  (is (thrown? ExceptionInfo
        (#'polyomino-enclosure.engine/area-size (#'polyomino-enclosure.engine/board (-> [[0 0] [1 0] [2 0]]
                                                                                        (#'polyomino-enclosure.engine/operate-polyomino #'polyomino-enclosure.engine/translate 1 1)))))))

;; (deftest test-create-programs
;;   (is (= [[[:rotate-counter-clockwise]  ; キーワードから関数に変えたら、テストしづらくなっちゃった……。どうしよう？
;;            [:translate 0 1]]
;;           [[:flip-vertical]
;;            [:translate -4 3]]
;;           []
;;           [[:flip-vertical]
;;            [:translate -5 -1]]
;;           [[:flip-horizontal]
;;            [:translate -1 1]]
;;           [[:translate 1 -3]
;;            [:rotate-clockwise]
;;            [:rotate-clockwise]
;;            [:rotate-clockwise]
;;            [:rotate-clockwise]]
;;           [[:translate -3 -4]]]
;;          (create-programs program-definition-strings))))

(deftest test-validate-game
  (is (= nil
         (validate-game [[[0 0] [0 1] [0 2] [1 2]]
                         [[0 0] [0 1] [0 2] [0 3]]]
                        [])))
  (is (= "原点を含まないポリオミノがあります。"
         (validate-game [[[0 0] [0 1] [0 2] [1 2]]
                         [[0 1] [0 2] [0 3] [0 4]]]
                        [])))
  (is (= "不正なポリオミノがあります。"
         (validate-game [[[0 0] [0 1] [0 2] [1 2]]
                         [[0 0] [0 1] [0 2]]]
                        [])))
  (is (= "不正なポリオミノがあります。"
         (validate-game [[[0 0] [0 1] [0 2] [1 2]]
                         [[0 0] [0 1] [0 2] [1 3]]]
                        [])))
  (is (= "1行目と4行目のポリオミノは重複しています。\n2行目と3行目のポリオミノは重複しています。"
         (validate-game [[[0 0] [0 1] [0 2] [1 2]]
                         [[0 0] [0 1] [0 2] [1 1]]
                         [[0 0] [1 0] [2 0] [1 1]]
                         [[0 0] [0 1] [0 2] [1 0]]]
                        []))))

(deftest test-execute-programs
  (is (= [[[[ 0  0] [ 0  1] [ 0  2] [ 0  3] [ 1  3]]
           [[ 0  0] [ 1  0] [ 2  0] [ 3  0] [ 3 -1]]
           [[ 0  1] [ 1  1] [ 2  1] [ 3  1] [ 3  0]]]
          [[[ 0  0] [ 0  1] [-1  1] [-1  2] [-1  3]]
           [[ 0  0] [ 0 -1] [-1 -1] [-1 -2] [-1 -3]]
           [[-4  3] [-4  2] [-5  2] [-5  1] [-5  0]]]
          nil
          [[[ 0  0] [ 0  1] [ 0  2] [ 1  2] [ 2  2]]
           [[ 0  0] [ 0 -1] [ 0 -2] [ 1 -2] [ 2 -2]]
           [[-5 -1] [-5 -2] [-5 -3] [-4 -3] [-3 -3]]]
          [[[ 0  0] [ 0  1] [ 1  1] [ 1  2] [ 2  2]]
           [[ 0  0] [ 0  1] [-1  1] [-1  2] [-2  2]]
           [[-1  1] [-1  2] [-2  2] [-2  3] [-3  3]]]
          [[[ 0  0] [ 1  0] [ 1  1] [ 1  2] [ 2  2]]
           [[ 1 -3] [ 2 -3] [ 2 -2] [ 2 -1] [ 3 -1]]
           [[ 3  1] [ 3  2] [ 2  2] [ 1  2] [ 1  3]]
           [[-1  3] [-2  3] [-2  2] [-2  1] [-3  1]]
           [[-3 -1] [-3 -2] [-2 -2] [-1 -2] [-1 -3]]
           [[ 1 -3] [ 2 -3] [ 2 -2] [ 2 -1] [ 3 -1]]]
          [[[ 0  0] [ 1  0] [ 2  0] [ 3  0] [ 4  0]]
           [[-3 -4] [-2 -4] [-1 -4] [ 0 -4] [ 1 -4]]]]
         (execute-programs (create-polyominos polyomino-definition-strings) (create-programs program-definition-strings)))))

(deftest test-validate-result
  (is (= nil
         (validate-result (result (execute-programs (create-polyominos polyomino-definition-strings)
                                                    (create-programs   program-definition-strings))))))
  (is (= "原点がポリオミノで覆われています。"
         (validate-result (result (execute-programs (create-polyominos polyomino-definition-strings)
                                                    (create-programs   (concat (butlast program-definition-strings)
                                                                               ["translate(-2,0);"])))))))
  (is (= "ポリオミノが重なっています。"
         (validate-result (result (execute-programs (create-polyominos polyomino-definition-strings)
                                                    (create-programs   (concat (butlast program-definition-strings)
                                                                               ["translate(-3,-3);"])))))))
  (is (= "ポリオミノで囲み込めていません。"
         (validate-result (result (execute-programs (create-polyominos polyomino-definition-strings)
                                                    (create-programs   (concat (butlast program-definition-strings)
                                                                               ["translate(-2,-4);"]))))))))

(deftest test-score
  (is (= 26 (score (result (execute-programs (create-polyominos polyomino-definition-strings) (create-programs program-definition-strings)))))))
