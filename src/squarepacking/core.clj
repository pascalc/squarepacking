(ns squarepacking.core
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

;; We have N squares, of sizes 1x1, 2x2, ... NxN
(def N 5)

;; Each coordinate is between 0 and N*(N+1)/2
(def MAX-COORD
  (-> N 
    (* (+ N 1))
    (/ 2)))

;; We represent our squares in a list: 
; [ [[x1 y1] [x1 + 1, y1 + 1]] 
;;  [[x2 y2] [x2 + 2, y2 + 2]],
;;   ... ]
;; where each square is represented by the 
;; coordinates of its top-left and
;; bottom-right corners

(defn make-square []
  (let [[x1 y1 x2 y2] (repeatedly 4 lvar)]
    [[x1 y1] [x2 y2]]))

(defn set-domain [[[x1 y1] [x2 y2]]]
  (fd/in x1 y1 x2 y2 (fd/interval 0 MAX-COORD)))

(defne constrain-coords [size square]
  ([s [[x1 y1] [x2 y2]]]
    (fd/eq 
      (= (+ x1 size) x2)
      (= (+ y1 size) y2))))

(defne constrain-squares [size squares]
  ([_ []])
  ([s [sqr . sqrs]]
    (constrain-coords s sqr)
    (project [s]
      (constrain-squares (inc s) sqrs))))

(defne not-overlapping-squares-o
  [sq1 sq2]
  ([[[x11 y11] [x12 y12]]
    [[x21 y21] [x22 y22]]]
    (conde
      [(fd/>= y21 y12)]
      [(fd/>= x21 x12)]
      [(fd/<= x22 x11)]
      [(fd/<= y22 y11)])))

(defne not-overlapping-with-o
  [square squares]
  ([_ []])
  ([sq1 [sq2 . sqrs]]
    (not-overlapping-squares-o sq1 sq2)
    (not-overlapping-with-o sq1 sqrs)))

(defne pairwise-not-overlapping-o 
  [squares]
  ([[]])
  ([[sq . sqrs]]
    (not-overlapping-with-o sq sqrs)
    (pairwise-not-overlapping-o sqrs)))

(defn pack! []
  (let [squares (vec (repeatedly N make-square))]
    (run 1 [q]
      (everyg set-domain squares)
      (constrain-squares 1 squares)
      (pairwise-not-overlapping-o squares)
      (== q squares))))

