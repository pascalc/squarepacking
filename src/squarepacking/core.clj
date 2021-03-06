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

(defne closest-square-acc [closest acc squares]
  ([ sq sq [] ])
  ([ sq [[x11 y11] [x12 y12]] [[[x21 y21] [_ _]] . sqrs] ]
    (fd/<= x11 x21)
    (fd/<= y11 y21)
    (closest-square-acc sq [[x11 y11] [x12 y12]] sqrs))
  ([ sq [[x11 y11] [_ _]] [[[x21 y21] [x22 y22]] . sqrs] ]
    (fd/> x11 x21)
    (fd/> y11 y21)
    (closest-square-acc sq [[x21 y21] [x22 y22]] sqrs)))

(defne closest-square-o [closest squares]
  ([ c [sq . sqrs] ] 
    (closest-square-acc c sq sqrs)))

(defne farthest-square-acc [farthest acc squares]
  ([ sq sq [] ])
  ([ sq [[x11 y11] [x12 y12]] [[[x21 y21] [_ _]] . sqrs] ]
    (fd/> x11 x21)
    (fd/> y11 y21)
    (farthest-square-acc sq [[x11 y11] [x12 y12]] sqrs))
  ([ sq [[x11 y11] [_ _]] [[[x21 y21] [x22 y22]] . sqrs] ]
    (fd/<= x11 x21)
    (fd/<= y11 y21)
    (farthest-square-acc sq [[x21 y21] [x22 y22]] sqrs)))

(defne farthest-square-o [farthest squares]
  ([ c [sq . sqrs] ] 
    (farthest-square-acc c sq sqrs)))

(defne solution-size-o [size squares]
  ([s sqrs]
    (fresh [closest farthest 
            x11 y11 x22 y22 _1 _2]
      (closest-square   [[x11 y11] _1] sqrs)
      (farthest-square  [_2 [x22 y22]] sqrs)
      (project [x11 y11 x22 y22]
        (let [a (- y22 y11)
              b (- x22 x11)]
          (== s (-> (+ (* a a) (* b b)) Math/sqrt Math/ceil int)))))))

(defn pack []
  (let [squares (vec (repeatedly N make-square))]
    (run 1 [q]
      (everyg set-domain squares)
      (constrain-squares 1 squares)
      (pairwise-not-overlapping-o squares)
      (fresh [closest farthest size]
        (closest-square-o closest squares)
        (farthest-square-o farthest squares)
        (solution-size-o size squares)
        (== q {:squares   squares 
               :closest   closest
               :farthest  farthest
               :size      size})))))

(defn solution-size []
  (run 1 [q]
    (solution-size-o q [[[0 0] [1 1]] [[1 1] [2 2]]])))

(defn constrained-solution-size []
  (run 1 [q] 
    (fresh [size x11 y11 
                 x12 y12 
                 x21 y21 
                 x22 y22 squares]
      (fd/in x11 y11 x12 y12 x21 y21 x22 y22 (fd/interval 0 2))
      (fd/eq 
        (= x11 0) (= y11 0) (= x21 1) (= y21 1)
        (= x12 (+ x11 1)) (= y12 (+ y11 1))
        (= x22 (+ x21 1)) (= y22 (+ y21 1)))
      (== squares [[[x11 y11] [x12 y12]] [[x21 y21] [x22 y22]]])
      (solution-size-o size squares)
      (== q {:squares squares :size size}))))

(defn unconstrained-solution-size []
  (run 1 [q] 
    (fresh [size x11 y11 
                 x12 y12 
                 x21 y21 
                 x22 y22 squares]
      (fd/in x11 y11 x12 y12 x21 y21 x22 y22 (fd/interval 0 2))
      (fd/eq 
        (= x11 0) (= y11 0) (= x21 1) ;(= y21 1)
        (= x12 (+ x11 1)) (= y12 (+ y11 1))
        (= x22 (+ x21 1)) (= y22 (+ y21 1)))
      (== squares [[[x11 y11] [x12 y12]] [[x21 y21] [x22 y22]]])
      (solution-size-o size squares)
      (== q {:squares squares :size size}))))


;; defne solution-size => (fd/eq "farthest" bottom-right - "closest" top-left)
;; (pack! better-than) sets constraint solution-size fd/< better-than => returns solution and solution-size
;; solution-size becomes next better-than

;; Project not working example:
; (run 1 [q] (fresh [x y z] (== x 0) (== y 1) (project [x y] (== z (+ x y)) (== z q))))
; => (1)
; (run 1 [q] (fresh [x y z] (fd/in x y (fd/interval 0 2)) (fd/eq (= y (+ x 1))) (project [x y] (== z (+ x y)) (== z q))))
; => ClassCastException clojure.core.logic.LVar cannot be cast to java.lang.Number  clojure.lang.Numbers.add (Numbers.java:126)

