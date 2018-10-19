(ns simplify.triangle
  (:require [simplify.vector :as vector]))

(defrecord Triangle [v1 v2 v3])

(defn normal [t]
  (let [e1 (vector/sub (:v2 t) (:v1 t))
        e2 (vector/sub (:v3 t) (:v1 t))]
    (vector/normalize (vector/cross e1 e2))))

(defn quadric [t]
  (let [n (normal t)
        [x y z] [(-> t :v1 :x) (-> t :v1 :y) (-> t :v1 :z)]
        [a b c] [(:x n) (:y n) (:z n)]
        d (- (* (* -1 a) x)
             (* b y)
             (* c z))]
    {:x00 (* a a)
     :x01 (* a b)
     :x02 (* a c)
     :x03 (* a d)

     :x10 (* a b)
     :x11 (* b b)
     :x12 (* b c)
     :x13 (* b d)

     :x20 (* a c)
     :x21 (* b c)
     :x22 (* c c)
     :x23 (* c d)

     :x30 (* a d)
     :x31 (* b d)
     :x32 (* c d)
     :x33 (* d d)}))
