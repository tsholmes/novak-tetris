(ns novak-tetris.game
  (use [quil.core]
       [novak-tetris.const]
       [novak-tetris.util]))

(defn next-piece []
  (let [pdef (nth piecedefs (random 7))
        startrot (first (pdef :rots))
        startshape (startrot :shape)
        hei (count startshape)
        wid (count (first startshape))]
    {:x (- 5 (int (/ wid 2)))
     :y ((startrot :center) 1)
     :shape startrot
     :rots (rest (pdef :rots))
     :color (pdef :color)}))

(defn new-board []
  {:board '()
   :piece (next-piece)})

(defn next-board-piece [board]
  (assoc board :piece (next-piece)))

(defn board-row [board y]
  (let [rows (board :board)
        toprow (- 20 (count rows))
        i (- y toprow)]
    (if (< i 0)
      (repeat 10 nil)
      (nth rows i))))

(defn board-row-mask [board y]
  (smap #(if (nil? %) 0 1) (board-row board y)))

(defn piece-row [piece y]
  (let [center (get-in piece [:shape :center])
        shape (get-in piece [:shape :shape])
        x1 (- (piece :x) (center 0))
        x2 (+ x1 (count (first shape)) -1)
        y1 (- (piece :y) (center 1))
        y2 (+ y1 (count shape) -1)
        ny (- y y1)
        cr (piece :color)]
    (if (or (< y y1) (> y y2))
      (repeat 10 nil)
      (smap
       (fn [x]
         (if (or (< x x1) (> x x2))
           nil
           (if (= 1 (grid-at (get-in piece [:shape :shape]) [(- x x1) ny]))
             cr
             nil)))
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
  (smap (fn [a b] (if (nil? a) b a)) r1 r2))

(defn stop-piece [board]
  (let [br (smap #(board-row board %) (range 20))
        pcr (smap #(piece-row (board :piece) %) (range 20))
        mr (smap merge-rows br pcr)
        ar (drop-while #(apply = nil %) mr)
        fr (filter #(not (not-any? nil? %)) ar)]
    {:board (if (>= (count fr) 20) '() fr) :piece (next-piece)}))

(defn check-overlap [board]
  (let [pc (board :piece)
        br (smap #(board-row-mask board %) (range 20))
        pcr (smap #(piece-row-mask pc %) (range 20))
        ov (some #(= 2 %) (map + (flatten br) (flatten pcr)))]
    (-> ov nil? not)))

(defn check-drop [board]
  (let [pc (board :piece)
        center (get-in pc [:shape :center])
        y (- (pc :y) (center 1))
        y2 (+ y (count (get-in pc [:shape :shape])) -1)]
    (if (or (>= y2 20) (check-overlap board))
      (stop-piece (dec-board board))
      board)))

(defn inc-board [board]
  (check-drop (reassoc board :piece inc-piece)))

(defn rot-piece [board]
  (let [pc (board :piece)
        cr (pc :shape)
        nr (first (pc :rots))
        nrs (concat (rest (pc :rots)) [cr])
        nb (assoc board :piece
             {:x (pc :x)
              :y (pc :y)
              :shape nr
              :rots nrs
              :color (pc :color)})]
    (if (check-overlap nb)
      board
      nb)))

(defn rot-back-piece [board]
  (let [pc (board :piece)
        cr (pc :shape)
        nr (last (pc :rots))
        nrs (concat [cr] (butlast (pc :rots)))
        nb (assoc board :piece
             {:x (pc :x)
              :y (pc :y)
              :shape nr
              :rots nrs
              :color (pc :color)})]
    (if (check-overlap nb)
      board
      nb)))

(defn move-piece-left [board]
  (let [pc (board :piece)
        center (get-in pc [:shape :center])
        x (- (pc :x) (center 0))
        np (reassoc pc :x dec)
        nb (assoc board :piece np)]
    (if (or (<= x 0) (check-overlap nb))
      board
      nb)))

(defn move-piece-right [board]
  (let [pc (board :piece)
        center (get-in pc [:shape :center])
        shape (get-in pc [:shape :shape])
        x (- (pc :x) (center 0))
        x2 (+ x (count (first shape)) -1)
        np (reassoc pc :x inc)
        nb (assoc board :piece np)]
    (if (or (>= x2 9) (check-overlap nb))
      board
      nb)))
