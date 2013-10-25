(ns novak-tetris.util
  (:use quil.core))

(defn maprun [f & ls]
  (dorun (apply map f ls)))

(defn gridrun [wid hei f]
  (maprun
   (fn [y]
     (maprun
      (fn [x]
        (f x y))
      (range wid)))
   (range hei)))
