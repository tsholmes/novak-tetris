(ns novak-tetris.game
  (use [quil.core]
       [novak-tetris.const]
       [novak-tetris.util]))

(defn next-piece []
  (let [pd (nth piecedefs (random 7))
        pc ((first (pd :rots)) :shape)
        hei (count pc)
        wid (count (first pc))]
    {:x (- 5 (int (/ wid 2)))
     :y (((first (pd :rots)) :center) 1)
     :shape (first (pd :rots))
     :rots (rest (pd :rots))
     :color (pd :color)}))

(defn new-board []
  {:board '()
   :piece (next-piece)})

(defn next-board-piece [board]
  (assoc board :piece (next-piece)))

(defn rot-piece [board]
  (let [pc (board :piece)
        cr (pc :shape)
        nr (first (pc :rots))
        nrs (concat (rest (pc :rots)) [cr])]
    (assoc board :piece
      {:x (pc :x)
       :y (pc :y)
       :shape nr
       :rots nrs
       :color (pc :color)})))

(defn rot-back-piece [board]
  (let [pc (board :piece)
        cr (pc :shape)
        nr (last (pc :rots))
        nrs (concat [cr] (butlast (pc :rots)))]
    (assoc board :piece
      {:x (pc :x)
       :y (pc :y)
       :shape nr
       :rots nrs
       :color (pc :color)})))

(defn check-drop [board] ; TODO: drop piece
  (let [pc (board :piece)]
    board))

(defn inc-piece [piece]
  (assoc piece :y (inc (piece :y))))

(defn inc-board [board]
  (check-drop (assoc board :piece (inc-piece (board :piece)))))
