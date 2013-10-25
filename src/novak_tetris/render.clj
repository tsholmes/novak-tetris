(ns novak-tetris.render
  (:use [quil.core]
        [novak-tetris.util]
        [novak-tetris.const]))

(defn drawtile [cr_]
  (let [cr (color (first cr_) (second cr_) (nth cr_ 2))
        white (color 255 255 255)
        black (color 0 0 0)
        lt (lerp-color cr white 0.2)
        dk1 (lerp-color cr black 0.3)
        dk2 (lerp-color cr black 0.6)]
    (fill cr)
    (rect 0 0 1 1)
    (fill lt)
    (quad 0 0 1 0 0.8 0.2 0.2 0.2)
    (fill dk1)
    (quad 0 0 0.2 0.2 0.2 0.8 0 1)
    (quad 1 0 1 1 0.8 0.8 0.8 0.2)
    (fill dk2)
    (quad 0 1 0.2 0.8 0.8 0.8 1 1)))

(defn draw-piece [pc]
  (push-matrix)
  (translate (pc :x) (pc :y))

  (let [shape (pc :shape)
        sh (shape :shape)
        hei (count sh)
        wid (count (first sh))
        cr (pc :color)]
    (translate (- ((shape :center) 0)) (- ((shape :center) 1)))
    (gridrun wid hei
             (fn [x y]
               (if (= 1 (get-in sh [y x]))
                 (do
                   (push-matrix)
                   (translate x y)
                   (drawtile cr)
                   (pop-matrix))))))

  (pop-matrix))

(defn draw-board [board] ; TODO: Draw rows
  (draw-piece (board :piece)))
