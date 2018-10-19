(ns simplify.vector)

(defrecord Vector [x y z])

(defn less [a b]
  (cond (not= (:x a) (:x b)) (< (:x a) (:x b))

        (not= (:y a) (:y b)) (< (:y a) (:y b))

        :else (< (:z a) (:z b))))

(defn length [a]
  (.sqrt js/Math (+ (* (:x a) (:x a))
                    (* (:y a) (:y a))
                    (* (:z a) (:z a)))))
(defn dot [a b]
  (+ (* (:x a) (:x b))
     (* (:y a) (:y b))
     (* (:z a) (:z b))))

(defn cross [a b]
  (let [x (- (* (:y a) (:z b))
             (* (:z a) (:y b)))
        y (- (* (:z a) (:x b))
             (* (:x a) (:z b)))
        z (- (* (:x a) (:y b))
             (* (:y a) (:x b)))]
    {:x x
     :y y
     :z z}))

(defn normalize [a]
  (let [d (length a)]
    {:x (/ (:x a) d)
     :y (/ (:y a) d)
     :z (/ (:z a) d)}))

(defn add [a b]
  {:x (+ (:x a) (:x b))
   :y (+ (:y a) (:y b))
   :z (+ (:z a) (:z b))})

(defn sub [a b]
  {:x (- (:x a) (:x b))
   :y (- (:y a) (:y b))
   :z (- (:z a) (:z b))})

(defn mul-scalar [a b]
  {:x (* (:x a) b)
   :y (* (:y a) b)
   :z (* (:z a) b)})
