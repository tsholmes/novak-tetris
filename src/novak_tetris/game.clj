(ns novak-tetris.game
  (:use [novak-tetris.const]
        [novak-tetris.util]))

(defn piece-sz [piece]
  (count (piece :shape)))

(defn piece-bounds [piece]
  (let [shape (piece :shape)
        shape-t (apply map vector shape)
        size (piece-sz piece)
        x1 (piece :x)
        y1 (piece :y)
        x2 (+ x1 size -1)
        y2 (+ y1 size -1)
        empty-count (fn [x] (count (take-while #(apply = 0 %) x)))
        top-off (empty-count shape)
        bottom-off (empty-count (reverse shape))
        left-off (empty-count shape-t)
        right-off (empty-count (reverse shape-t))]
    {:left (+ x1 left-off)
     :right (- x2 right-off)
     :top (+ y1 top-off)
     :bottom (- y2 bottom-off)}))

(defn center-drop-piece [piece]
  (let [shape (piece :shape)
        size (piece-sz piece)
        mid (int (/ size 2))]
    (assoc piece :x (- 5 mid) :y (- mid))))

(defn next-piece []
  (let [pdef (nth piecedefs (rnd 7))
        color (pdef :color)
        shape (pdef :shape)
        kicks (pdef :kick)]
    (center-drop-piece
     {:x 0
      :y 0
      :shape shape
      :color color
      :rot 0
      :kick kicks})))

(defn new-board []
  {:board '()
   :piece (next-piece)
   :hold nil
   :swap true})

(defn next-board-piece [board]
  (assoc board :piece (next-piece)))

(defn board-row [board y]
  (let [rows (board :board)
        toprow (- 20 (count rows))
        i (- y toprow)]
    (if (neg? i)
      (repeat 10 nil)
      (nth rows i))))

(defn board-row-mask [board y]
  (smap #(if (nil? %) 0 1) (board-row board y)))

(defn piece-row [piece y]
  (let [shape (piece :shape)
        size (piece-sz piece)
        x1 (piece :x)
        y1 (piece :y)
        x2 (+ x1 size -1)
        y2 (+ y1 size -1)
        ny (- y y1)
        color (piece :color)]
    (if (or (< y y1) (> y y2))
      (repeat 10 nil)
      (smap
       (fn [x]
         (when-not (or (< x x1) (> x x2))
           (when (= 1 (grid-at shape [(- x x1) ny]))
             color)))
       (range 10)))))

(defn piece-row-mask [piece y]
  (smap #(if (nil? %) 0 1) (piece-row piece y)))

(defn inc-piece [piece]
  (reassoc piece :y inc))

(defn dec-piece [piece]
  (reassoc piece :y dec))

(defn dec-board [board]
  (reassoc board :piece dec-piece))

(defn merge-rows [r1 r2]
  (smap (fn [a b] (or a b)) r1 r2))

(defn stop-piece [board]
  (let [piece (board :piece)
        br (smap #(board-row board %) (range 20))
        pcr (smap #(piece-row piece %) (range 20))
        mr (smap merge-rows br pcr)
        ar (drop-while #(apply = nil %) mr)
        fr (filter #(not (not-any? nil? %)) ar)]
    (if (>= (count fr) 20)
      (new-board)
      (assoc board :board fr :piece (next-piece) :swap true))))

(defn check-overlap [board]
  (let [pc (board :piece)
        br (smap #(board-row-mask board %) (range 20))
        pcr (smap #(piece-row-mask pc %) (range 20))
        ov (some #(= 2 %) (map + (flatten br) (flatten pcr)))]
    (-> ov nil? not)))

(defn check-oob [piece]
  (let [shape (piece :shape)
        size (piece-sz piece)
        bounds (piece-bounds piece)
        x (bounds :left)
        x2 (bounds :right)
        y (bounds :top)
        y2 (bounds :bottom)]
    (or (neg? x) (>= x2 10) (>= y2 20))))

(defn check-drop [board]
  (let [piece (board :piece)
        bounds (piece-bounds piece)]
    (if (or (>= (bounds :bottom) 20) (check-overlap board))
      (stop-piece (dec-board board))
      board)))

(defn full-drop [board]
  (let [nb (assoc board :piece (reassoc (board :piece) :y inc))]
    (if (or (check-oob (nb :piece)) (check-overlap nb))
      board
      (recur nb))))

(defn inc-board [board]
  (check-drop (reassoc board :piece inc-piece)))

(defn kick-piece [board kick]
  (let [piece (board :piece)
        x (piece :x)
        y (piece :y)]
    (assoc board :piece (assoc piece :x (+ x (kick 0)) :y (+ y (kick 1))))))

(defn rot-piece [board]
  (let [piece (board :piece)
        shape (piece :shape)
        cw-rot (apply map vector (reverse shape))
        cur-rot (piece :rot)
        next-rot (mod (+ cur-rot 1) 4)
        kicks (nth (get-in piece [:kick :cw]) cur-rot)
        np (assoc piece :shape cw-rot :rot next-rot)
        nb (assoc board :piece np)
        kicked-boards (map #(kick-piece nb %) kicks)
        valid-kick (some #(when (not (or (check-overlap %1) (check-oob (%1 :piece)))) %1) kicked-boards)]
    (or valid-kick board)))

(defn rot-back-piece [board]
  (let [piece (board :piece)
        shape (piece :shape)
        ccw-rot (reverse (apply map vector shape))
        cur-rot (piece :rot)
        next-rot (mod (+ cur-rot 3) 4)
        kicks (nth (get-in piece [:kick :ccw]) cur-rot)
        np (assoc piece :shape ccw-rot :rot next-rot)
        nb (assoc board :piece np)
        kicked-boards (map #(kick-piece nb %) kicks)
        valid-kick (some #(when (not (or (check-overlap %1) (check-oob (%1 :piece)))) %1) kicked-boards)]
    (or valid-kick board)))

(defn rot-back-piece [board]
  (let [piece (board :piece)
        shape (piece :shape)
        ccw-rot (reverse (apply map vector shape))
        np (assoc piece :shape ccw-rot)
        nb (assoc board :piece np)]
    (if (or (check-overlap nb) (check-oob np))
      board
      nb)))

(defn move-piece-left [board]
  (let [piece (board :piece)
        np (reassoc piece :x dec)
        nb (assoc board :piece np)]
    (if (or (check-oob np) (check-overlap nb))
      board
      nb)))

(defn move-piece-right [board]
  (let [piece (board :piece)
        np (reassoc piece :x inc)
        nb (assoc board :piece np)]
    (if (or (check-oob np) (check-overlap nb))
      board
      nb)))

(defn hold-piece [board]
  (if (nil? (board :hold))
    (assoc board :hold (center-drop-piece (board :piece)) :piece (next-piece) :swap false)
    (let [hold (board :hold)
          piece (board :piece)]
      (if (board :swap)
        (assoc board :hold (center-drop-piece piece) :piece (center-drop-piece hold) :swap false)
        board))))
