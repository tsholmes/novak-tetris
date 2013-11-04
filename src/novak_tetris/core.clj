(ns novak-tetris.core
  (:gen-class)
  (:use [quil.core]
        [novak-tetris.util]
        [novak-tetris.const]
        [novak-tetris.render]
        [novak-tetris.game]))

(def all-games (atom nil))
(def keys-pressed (atom '()))

(defn get-keys []
  (let [ks @keys-pressed]
    (swap! keys-pressed empty)
    ks))

(defn setup []
  (smooth)
  (frame-rate 30)
  (background 0))

(defn draw []
  (no-stroke)

  (fill 0)
  (rect 0 0 (width) (height))

  (push-matrix)

  (let [wid (width)
        hei (height)
        gcount (count @all-games)
        mul (min (/ wid (* 20 gcount)) (/ hei 20))
        next-games (map
                    (fn [game]
                      (let [board (game :board)
                            update (game :update)]
                        (assoc game :board (update board))))
                    @all-games)]
    (scale mul)
    (maprun-indexed
     (fn [index game]
       (push-matrix)
       (translate (+ 5 (* index 20)) 0)
       (draw-board (game :board))
       (pop-matrix))
     next-games)
    (reset! all-games next-games))

  (pop-matrix))

(defn key-pressed []
  (let [k (key-code)]
    (swap! keys-pressed #(concat % (list k)))))

(defn inc-user-board [_board]
  (if (nil? (_board :counter))
    (recur (assoc _board :counter (atom 0)))
    (let [counter (_board :counter)
          board (atom _board)
          inc-game (fn [] (swap! board inc-board) (reset! counter 0))]
      (maprun
       (fn [k]
         (case k
           65 (swap! board move-piece-left) ; A
           68 (swap! board move-piece-right) ; D
           83 (inc-game) ; S
           81 (swap! board rot-back-piece) ; Q
           69 (swap! board rot-piece) ; E
           32 (do (swap! board full-drop) (inc-game)) ; Space
           16 (swap! board hold-piece) ; Shift
           nil))
       (get-keys))
      (if (>= @counter 20)
        (do
          (swap! board inc-board)
          (reset! counter 0))
        (swap! counter inc))
      @board)))

(defn new-user-board []
  {:board (new-board) :update inc-user-board})

(defn run-games [& games]
  (reset! all-games games)
  (defsketch tetris
    :title "NOVAK TETRIS"
    :setup setup
    :draw draw
    :key-pressed key-pressed
    :size [(* 400 (count games)) 400]))

(defn -main []
  (run-games (new-user-board)))


