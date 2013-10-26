(ns novak-tetris.core
  (:gen-class)
  (:use [quil.core]
        [novak-tetris.util]
        [novak-tetris.const]
        [novak-tetris.render]
        [novak-tetris.game]))

(def board (atom nil))
(def counter (atom 0))

(defn setup []
  (smooth)
  (frame-rate 30)
  (background 0)
  (reset! board (new-board)))

(defn draw []
  (no-stroke)

  (fill 0)
  (rect 0 0 (width) (height))

  (push-matrix)

  (let [wid (width)
        hei (height)
        mul (min (/ wid 16) (/ hei 20))]
    (scale mul)
    (translate 3 0))

  (if (>= @counter 20)
    (do
      (swap! board inc-board)
      (reset! counter 0))
    (swap! counter inc))

  (draw-board @board)

  (pop-matrix))

(defn key-pressed []
  (let [k (key-code)]
    (case k
      65 (swap! board move-piece-left) ; A
      68 (swap! board move-piece-right) ; D
      83 (swap! board inc-board) ; S
      81 (swap! board rot-back-piece) ; Q
      69 (swap! board rot-piece) ; E
      32 (swap! board #(-> % full-drop inc-board)) ; Space
      16 (swap! board hold-piece) ; Shift
      nil)))

(defn -main []
  (defsketch example
    :title "TETRIS SOON"
    :setup setup
    :draw draw
    :key-pressed key-pressed
    :size [480 600]))


