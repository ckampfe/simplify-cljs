(ns simplify.pair
  (:require [simplify.vector :as vector]
            [simplify.matrix :as matrix]))

(defrecord Pair [a b index removed cached-error])

(defrecord PairKey [a b])

(defn make-pair-key [a b]
  (if (vector/less b a)
    (->PairKey b a)
    (->PairKey a b)))

(defn new-pair [a b]
  (if (vector/less b a)
    (->Pair b a -1 false -1)
    (->Pair a b -1 false -1)))

(defn quadric [p]
  (matrix/add (-> p :a :quadric)
              (-> p :b :quadric)))

(defn continue-vector [p q]
  (let [n 32
        a (-> p :a :vector)
        b (-> p :b :vector)
        d (vector/sub b a)
        best-e -1.0
        best-v (vector/->Vector 0.0 0.0 0.0)]
    (loop [i 0
           beste best-e
           bestv best-v]
      (if (<= i n)
        (let [t (/ (double i) n)
              v (vector/add a (vector/mul-scalar d t))
              e (matrix/quadric-error q v)
              ]
          (if (or (< beste 0)
                  (< e beste))
            (recur (inc i) e v)
            (recur (inc i) beste bestv)))
        bestv))))

(defn pair-vector [p]
  (let [q (quadric p)]
    (if (> (.abs js/Math (matrix/determinant q)) 1e-3)
      (let [v (matrix/quadric-vector q)]
        (if (and (not (js/isNaN (:x v)))
              (not (js/isNaN (:y v)))
              (not (js/isNaN (:z v))))
          v
          (continue-vector p q)))
      (continue-vector p q))))


;; func (p *Pair) Error() float64 {
;;     if p.CachedError < 0 {
;;           p.CachedError = p.Quadric().QuadricError(p.Vector())
;;     }
;;     return p.CachedError
;; }
;;
;; should return a float, but we don't have mutable types
(defn error [p]
  (if (< (:cached-error p) 0)
    (assoc p
           :cached-error
           (matrix/quadric-error (quadric p)
                                 (:vector p)))
    p))
