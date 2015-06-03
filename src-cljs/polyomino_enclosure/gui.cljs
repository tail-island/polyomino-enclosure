(ns polyomino-enclosure.gui
  (:require        [clojure.string             :as    string]
                   [cljs.core.async            :refer [chan pub sub put! <!]]
                   [goog.dom                   :as    dom]
                   [goog.events                :as    events]
                   [om.core                    :as    om]
                   [sablono.core               :as    html :refer-macros [defelem html]]
                   [polyomino-enclosure.engine :as    engine])
  (:require-macros [cljs.core.async.macros     :refer [go-loop]])
  (:import         [goog.events                EventType]))

(enable-console-print!)

(def ^:private model
  (atom {:definition {:question   (str "0,0,0,1,0,2,0,3,1,3\n"
                                       "0,0,0,1,-1,1,-1,2,-1,3\n"
                                       "0,0,0,1,1,1,2,1,2,0\n"
                                       "0,0,0,1,0,2,1,2,2,2\n"
                                       "0,0,0,1,1,1,1,2,2,2\n"
                                       "0,0,1,0,1,1,1,2,2,2\n"
                                       "0,0,1,0,2,0,3,0,4,0\n")
                      :answer     (str "rotate_counter_clockwise();translate(0,1);\n"
                                       "flip_vertical();translate(-4,3);\n"
                                       "\n"
                                       "flip_vertical();translate(-5,-1);\n"
                                       "flip_horizontal();translate(-1,1);\n"
                                       "translate(1,-3);rotate_clockwise();rotate_clockwise();rotate_clockwise();rotate_clockwise();\n"
                                       "translate(-3,-4);\n")}
         :feedback   {:message    nil}
         :game       {:polyominos nil
                      :programs   nil
                      :result     nil}}))

(def ^:private event-publisher
  (chan))

(def ^:private event-publication
  (pub event-publisher :type))

(def ^:private line-colors
  (cycle ["#cc0000" "#cccc00" "#00cc00" "#00cccc" "#0000cc" "#cc00cc"]))

(def ^:private fill-colors
  (cycle ["#880000" "#888800" "#008800" "#008888" "#000088" "#880088"]))

(defelem nbsp
  [& [count]]
  [:span {:dangerouslySetInnerHTML #js {:__html (apply str (repeat (or count 1) "&nbsp;"))}}])

(defn- no-error?
  []
  (empty? (get-in @model [:feedback :message])))

(defn definition-view
  [{:keys [question answer] :as cursor} owner]
  (letfn [(clear [& keys]
            (doseq [key keys]
              (om/update! cursor key "")))]
    (om/component
     (html [:div
            [:div.row
             [:div.col-md-6
              [:div.form-group
               (html/label :question "問題")
               (html/text-area {:class "form-control", :rows "5", :on-change (fn [event] (om/update! cursor :question (-> event .-target .-value)))} :question question)]]
             [:div.col-md-6
              [:div.form-group
               (html/label :programs "解答")
               (html/text-area {:class "form-control", :rows "5", :on-change (fn [event] (om/update! cursor :answer   (-> event .-target .-value)))} :answer   answer)]]]
            [:div
             [:button.btn.btn-default {:on-click (fn [event] (clear :answer :question))} "クリア"]
             (nbsp)
             [:button.btn.btn-default {:on-click (fn [event] (clear :answer))} "解答のみクリア"]
             (nbsp)
             [:button.btn.btn-primary {:on-click (fn [event] (put! event-publisher {:type :prepare}) nil)} "実行"]]]))))

(defn feedback-view
  [{:keys [message] :as cursor} owner]
  (letfn [(finish []
            (when (no-error?)
              (om/update! cursor [:message] (str "正常終了しました。スコアは" (engine/score (get-in @model [:game :result])) "です。"))))]
    (reify
      om/IWillMount
      (will-mount [_]
        (let [events (sub event-publication :finish (chan))]
          (go-loop []
            (when (<! events)
              (finish))
            (recur))))
      om/IRender
      (render [_]
        (html [:div.form-group.has-error
               (html/label :message "メッセージ")
               (html/text-area {:class "form-control", :rows 3} :message message)])))))

(defn game-view
  [{:keys [polyominos programs result] :as cursor} owner]
  (let [stage (atom nil)]
    (letfn [(reset-stage []
              (reset! stage (js/createjs.Stage. (dom/getElement "canvas")))
              (doto js/createjs.Ticker
                (.setFPS 30)
                (.on     "tick" (fn [event]
                                  (when (.hasActiveTweens js/createjs.Tween)
                                    (.update @stage))))))
            (resize-canvas []
              (doto (.-canvas @stage)
                (-> .-width  (set! (- (.-innerWidth  js/window)                                         30)))
                (-> .-height (set! (- (.-innerHeight js/window) (.-clientHeight (dom/getElement "nav")) 30))))
              (doto @stage
                (-> .-x (set! (/ (-> @stage .-canvas .-width)  2)))
                (-> .-y (set! (/ (-> @stage .-canvas .-height) 2)))))
            (scale-stage-for [points]
              (let [[l1 t1 r1 b1] (let [[left top] (map #(- 0 (/ % 2)) ((juxt #(.-width %) #(.-height %)) (.-canvas @stage)))]
                                    [left top (- 0 left) (- 0 top)])
                    [l2 t2 r2 b2] (engine/left-top-right-and-bottom points)
                    scale         (apply min (filter identity [(and (< l2 0) (/ l1 (dec l2)))
                                                               (and (< t2 0) (/ t1 (dec t2)))
                                                               (and (> r2 0) (/ r1 (inc r2)))
                                                               (and (> b2 0) (/ b1 (inc b2)))]))]
                (doto @stage
                  (-> .-scaleX (set! scale))
                  (-> .-scaleY (set! scale)))))
            (block-shape [line-color fill-color [x y]]
              (doto (js/createjs.Shape.)
                (-> .-graphics
                    (.setStrokeStyle 0.1 "round")
                    (.beginStroke    line-color)
                    (.beginFill      fill-color)
                    (.rect           -0.45 -0.45 0.9 0.9))
                (-> .-x (set! x))
                (-> .-y (set! y))))
            (draw-deck [polyominos]
              (letfn [(draw-deck-polyominos [[polyomino & more-polyominos :as polyominos] [x & more-xs] [line-color & more-line-colors] [fill-color & more-fill-colors]]
                        (if polyominos
                          (let [polyomino-shape (doto (reduce #(doto %1
                                                                 (.addChild %2))
                                                              (js/createjs.Container.)
                                                              (map (partial block-shape line-color fill-color) polyomino))
                                                  (-> .-scaleX (set! 10))
                                                  (-> .-scaleY (set! 10)))]
                            (doto @stage
                              (.addChild polyomino-shape))
                            (-> js/createjs.Tween
                                (.get  polyomino-shape)
                                (.to   #js {:x      (- x (first (engine/left-top-right-and-bottom polyomino)))
                                            :scaleX 1
                                            :scaleY 1}
                                       200)
                                (.call #(draw-deck-polyominos more-polyominos more-xs more-line-colors more-fill-colors))))
                          (do (.update @stage)
                              (js/setTimeout #(put! event-publisher {:type :execute}) 500))))]
                (doto js/createjs.Tween
                  (.removeAllTweens))
                (doto @stage
                  (.removeAllChildren)
                  (.update))
                (let [ws (interpose 1 (map #(let [[l _ r _] (engine/left-top-right-and-bottom %)]
                                              (+ (- r l) 1))
                                           polyominos))
                      w  (reduce + ws)
                      xs (reduce #(conj %1 (reduce + (last %1) %2))
                                 [(- 0.5 (/ w 2))]
                                 (partition 2 ws))]
                  (scale-stage-for [[(- 0 (/ w 2)) 0] [(/ w 2) 0]])
                  (draw-deck-polyominos polyominos xs line-colors fill-colors))))
            (draw-execution [execute-program-result]
              (letfn [(draw-polyomino-steps [[polyomino-step & more-polyomino-steps :as polyomino-steps] [line-color & more-line-colors] [fill-color & more-fill-colors]]
                        (if polyomino-steps
                          (if polyomino-step
                            (-> (first (doall (map (fn [[point & more-points]]
                                                     (let [block-shape (block-shape line-color fill-color point)]
                                                       (doto @stage
                                                         (.addChild block-shape))
                                                       (reduce (fn [tween [x y]]
                                                                 (-> tween
                                                                     (.to #js {:x x, :y y} 200)))
                                                               (-> js/createjs.Tween
                                                                   (.get block-shape))
                                                               more-points)))
                                                   (apply map vector polyomino-step))))
                                (.call #(draw-polyomino-steps more-polyomino-steps more-line-colors more-fill-colors)))
                            (recur more-polyomino-steps more-line-colors more-fill-colors))  ; プログラムが指定されない場合はポリオミノを配置しない仕様なので、自前で再帰します。
                          (do (.update @stage)
                              (put! event-publisher {:type :finish}))))]
                (doto js/createjs.Tween
                  (.removeAllTweens))
                (doto @stage
                  (.removeAllChildren)
                  (.addChild (doto (js/createjs.Shape.)
                               (-> .-graphics
                                   (.setStrokeStyle 0.1 "round")
                                   (.beginStroke    "#808080")
                                   (.moveTo         -0.25  0)
                                   (.lineTo          0.25  0)
                                   (.moveTo          0    -0.25)
                                   (.lineTo          0     0.25))))
                  (.update))
                (scale-stage-for (mapcat (partial apply concat) execute-program-result))
                (draw-polyomino-steps execute-program-result line-colors fill-colors)))]
      (reify
        om/IWillMount
        (will-mount [_]
          (let [events (sub event-publication :draw-deck (chan))]
            (go-loop []
              (let [{:keys [polyominos]} (<! events)]
                (draw-deck polyominos))
              (recur)))
          (let [events (sub event-publication :draw-execution (chan))]
            (go-loop []
              (let [{:keys [execute-program-result]} (<! events)]
                (draw-execution execute-program-result))
              (recur))))
        om/IDidMount
        (did-mount [_]
          (reset-stage)
          (resize-canvas)
          (events/listen js/window (.-RESIZE EventType) resize-canvas))
        om/IRender
        (render [_]
          (html [:div
                 [:canvas#canvas]]))))))

(defn view
  [{:keys [definition feedback game] :as cursor} owner]
  (letfn [(prepare []
            (let [polyominos (engine/create-polyominos (string/split-lines (get-in @model [:definition :question])))
                  programs   (engine/create-programs   (string/split-lines (get-in @model [:definition :answer])))
                  error      (engine/validate-game polyominos programs)]
              (om/update! cursor [:game     :polyominos] polyominos)
              (om/update! cursor [:game     :programs]   programs)
              (om/update! cursor [:feedback :message]    (or error ""))
              (put! event-publisher {:type :draw-deck, :polyominos polyominos})))
          (execute []
            (when (no-error?)
              (let [execute-program-result (engine/execute-programs (get-in @model [:game :polyominos]) (get-in @model [:game :programs]))
                    result                 (engine/result execute-program-result)
                    error                  (engine/validate-result result)]
                (om/update! cursor [:game     :result]  result)
                (om/update! cursor [:feedback :message] (or error ""))
                (put! event-publisher {:type :draw-execution, :execute-program-result execute-program-result}))))]
    (reify
      om/IWillMount
      (will-mount [_]
        (let [events (sub event-publication :prepare (chan))]
          (go-loop []
            (when (<! events)
              (prepare))
            (recur)))
        (let [events (sub event-publication :execute (chan))]
          (go-loop []
            (when (<! events)
              (execute))
            (recur))))
      om/IRender
      (render [_]
        (html [:div.container-fluid
               [:nav#nav
                (om/build definition-view definition)
                [:hr]
                (om/build feedback-view feedback)
                [:hr]]
               [:article#article
                (om/build game-view game)]])))))

(om/root view model {:target (dom/getElement "app")})

;; TODO: イベント・ループのところをマクロ化する。
;; TODO: scale-stage-forをリファクタリングする。今のコードは格好悪い。

