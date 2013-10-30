(ns novak-tetris.render
  (:use [quil.core]
        [novak-tetris.util]
        [novak-tetris.const]
        [novak-tetris.game]))

(defn draw-tile [cr_]
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

(defn draw-tile-border [cr_]
  (let [cr (color (first cr_) (second cr_) (nth cr_ 2))]
    (no-fill)
    (stroke cr)
    (stroke-weight 0.125)
    (rect 0 0 1 1)
    (no-stroke)))

(defn draw-board [board]
  (maprun
   (fn [y]
     (let [brow (board-row board y)
           prow (piece-row (board :piece) y)
           mrow (merge-rows brow prow)]
       (maprun
        (fn [x]
          (if (not (nil? (nth mrow x)))
            (do
              (push-matrix)
              (translate x y)
              (draw-tile (nth mrow x))
              (pop-matrix))))
        (range 10))))
   (range 20))
  (let [db (full-drop board)]
    (maprun
     (fn [y]
       (let [prow (piece-row (db :piece) y)]
         (maprun
          (fn [x]
            (if (nth prow x)
              (do
                (push-matrix)
                (translate x y)
                (draw-tile-border (nth prow x))
                (pop-matrix))))
          (range 10))))
     (range 20)))
  (if (not (nil? (board :hold)))
    (let [hold (assoc (board :hold) :x 0 :y 0)
          hshape (hold :shape)
          size (piece-sz hold)
          half-size (/ size 2)
          bounds (piece-bounds hold)
          hb {:board '() :piece hold}]
      (push-matrix)
      (translate (- -2.5 half-size) (- 2.5 half-size))
      (maprun
       (fn [y]
         (let [prow (piece-row (hb :piece) y)]
           (maprun
            (fn [x]
              (if (nth prow x)
                (do
                  (push-matrix)
                  (translate x y)
                  (draw-tile (nth prow x))
                  (pop-matrix))))
            (range 10))))
       (range 20))
      (pop-matrix)))

  (push-style)
  (no-fill)
  (stroke 255)
  (stroke-weight 0.125)
  (rect 0 0 10 20)
  (rect -5 0 5 5)
  (pop-style))


