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

(defn reassoc [m k f]
  (assoc m k (f (m k))))

(defn grid-at [g [x y]]
  (nth (nth g y) x))

(defn smap [f & ls]
  (seq (apply map f ls)))
