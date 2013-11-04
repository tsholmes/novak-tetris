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

(defn draw-full-board [board]
  (maprun
   (fn [y]
     (let [brow (board-row board y)
           prow (piece-row (board :piece) y)
           mrow (merge-rows brow prow)]
       (maprun
        (fn [x]
          (when (nth mrow x)
            (push-matrix)
            (translate x y)
            (draw-tile (nth mrow x))
            (pop-matrix)))
        (range 10))))
   (range 20)))

(defn draw-piece-with [piece drawfunc]
  (push-matrix)
  (translate (piece :x) (piece :y))
  (let [pc (assoc piece :x 0 :y 0)
        size (piece-sz piece)]
    (maprun
     (fn [y]
       (let [prow (piece-row pc y)]
         (maprun
          (fn [x]
            (when (nth prow x)
              (push-matrix)
              (translate x y)
              (drawfunc (nth prow x))
              (pop-matrix)))
          (range size))))
     (range size)))
  (pop-matrix))

(defn draw-piece [piece]
  (draw-piece-with piece draw-tile))

(defn draw-piece-border [piece]
  (draw-piece-with piece draw-tile-border))

(defn with-piece-centered [drawfunc piece]
  (push-matrix)
  (let [half-size (/ (piece-sz piece) 2)]
    (translate (- half-size) (- half-size))
    (drawfunc piece))
  (pop-matrix))

(defn draw-board [board]
  (draw-full-board board)

  (draw-piece-border ((full-drop board) :piece))

  (when (board :hold)
    (push-matrix)
    (translate -2.5 2.5)

    (with-piece-centered draw-piece (assoc (board :hold) :x 0 :y 0))

    (pop-matrix))

  (maprun-indexed
   (fn [index piece]
     (push-matrix)
     (translate 12.5 (+ 2.5 (* index 5)))

     (with-piece-centered draw-piece piece)

     (pop-matrix))
   (board :queue))

  (push-style)
  (no-fill)
  (stroke 255)
  (stroke-weight 0.125)
  (rect 0 0 10 20)
  (rect -5 0 5 5)
  (rect -5 0 20 20)
  (pop-style))

