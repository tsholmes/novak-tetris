(ns novak-tetris.core
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

  (let [mul (min (/ (width) 10) (/ (height) 20))]
    (scale mul))

  (if (= 30 @counter)
    (do
      (swap! board inc-board)
      (reset! counter 0))
    (swap! counter inc))

  (draw-board @board)

  (pop-matrix))

(defn key-pressed []
  (let [k (key-code)]
    (case k
      39 (swap! board rot-piece)
      37 (swap! board rot-back-piece)
      40 (swap! board next-board-piece)
      nil)))

(defn -main []
  (defsketch example
    :title "TETRIS SOON"
    :setup setup
    :draw draw
    :key-pressed key-pressed
    :size [300 600]))


