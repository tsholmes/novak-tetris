(ns novak-tetris.core
  (:use quil.core))

(def colors [[0 255 255]
             [0 0 255]
             [255 127 0]
             [255 255 0]
             [0 255 0]
             [127 0 127]
             [255 0 0]])

(defn drawtile [i]
  (let [cr_ (nth colors i)
        cr (color (first cr_) (second cr_) (nth cr_ 2))
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

(defn setup []
  (smooth)
  (frame-rate 2)
  (background 0))

(defn draw []
  (no-stroke)
  (push-matrix)
  (scale 32)
  (dorun
   (map
    (fn [y]
      (dorun
       (map
        (fn [x]
          (push-matrix)
          (translate x y)
          (drawtile (mod (+ x y) 7))
          (pop-matrix))
          (range 16))))
      (range 16)))
  (pop-matrix))

(defn -main []
  (defsketch example
    :title "TETRIS SOON"
    :setup setup
    :draw draw
    :size [512 512]))


