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
    (distinct ks)))

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

(defn thread [x fs]
  (if (empty? fs) x (recur ((first fs) x) (rest fs))))

(defn thread-cond [x [f & fs]]
  (if f
    (if ((first f) x)
      (recur (thread x (-> (second f) list flatten)) fs)
      (recur x fs))
    x))

(defn inc-user-board [board]
  (thread-cond
   board
   (concat
    (list [#(nil? (% :counter)) #(assoc % :counter 0)])
    (map
     #(vector (constantly true) %)
     (smap
      (fn [k]
        (case k
          65 move-piece-left ; A
          68 move-piece-right ; D
          83 (list inc-board #(assoc % :counter 0)) ; S
          81 rot-back-piece ; Q
          69 rot-piece ; E
          32 (list full-drop inc-board #(assoc % :counter 0)) ; Space
          16 hold-piece ; Shift
          identity))
      (get-keys)))
    (list [#(>= (% :counter) 20) (list inc-board #(assoc % :counter 0))]
          [#(< (% :counter) 20) #(reassoc % :counter inc)]))))

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


