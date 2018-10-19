(ns simplify.util
  (:require [instaparse.core :as insta :refer-macros [defparser]]))

(defparser math "
S = Exp+ | Term
<Exp> = (Sub | Add) / (Div | Mul)
Div = Term (<Space> <'/'> <Space> Term)+
Mul = Term (<Space> <'*'> <Space> Term)+
Add = (Term | Mul | Div) (<Space> <'+'> <Space> (Term | Mul | Div | Sub))+
Sub = (Term | Mul | Div) (<Space> <'-'> <Space> (Term | Mul | Div | Add))+
Space = #\"\\s*\"
Term = #\"[\\w\\.\\d]+\" | FieldAccess
FieldAccess = #\"[\\w\\d]+\\.[\\w\\d]+\"
")

(def to-prefix {:Mul (fn [& args] (str "(* " (clojure.string/join " " args) ")"))
                :Add (fn [& args] (str "(+ " (clojure.string/join " " args) ")"))
                :Div (fn [& args] (str "(/ " (clojure.string/join " " args) ")"))
                :Sub (fn [& args] (str "(- " (clojure.string/join " " args) ")"))
                :Term identity
                :FieldAccess (fn [s] (let [[l r] (clojure.string/split s #"\.")]
                                       (str "(" ":" (clojure.string/lower-case r) " " l ")")))
                :S identity})

(comment

  ;; expecting: "(+ (:x00 a) (:x00 b))"
  (insta/transform to-prefix (insta/parse math "a.x00 + b.x00"))

  ;; expecting: "(+ (* (:x v) (:x03 a)) (* (:y v) (:x13 a)))"
  (insta/transform to-prefix (insta/parse math "v.X * a.x03 + v.Y * a.x13"))

  ;; input:
  ;;
  ;; v.X * a.x00 * v.X +
  ;; v.Y * a.x10 * v.X +
  ;; v.Z * a.x20 * v.X +
  ;; a.x30 * v.X
  ;;
  ;; hand-rolled clj output with field accesses translated:
  #_(+ (* (:x v) (:x00 a) (:x v))
       (* (:y v) (:x10 a) (:x v))
       (* (:z v) (:x20 a) (:x v))
       (* (:x30 a) (:x v)))
  ;; expecting:
  ;; "(+ (* (:x v) (:x00 a) (:x v)) (* (:y v) (:x10 a) (:x v)) (* (:z v) (:x20 a) (:x v)) (* (:x30 a) (:x v)))"
  (insta/transform to-prefix (insta/parse math "v.X * a.x00 * v.X + v.Y * a.x10 * v.X + v.Z * a.x20 * v.X + a.x30 * v.X"))

  ;; v.X * v.Y * v.Z + v.X / v.Y / v.Z
  ;; expecting: "(+ (* (:x v) (:y v) (:z v)) (/ (:x v) (:y v) (:z v)))"
  (insta/transform to-prefix (insta/parse math "v.X * v.Y * v.Z + v.X / v.Y / v.Z"))

  #_"(+ (* (:x00 a) (:x b)) (* (:x01 a) (:y b)) (* (:x02 a) (:z b)) (:x03 a))"

  #_"(+ (* (:x10 a) (:x b)) (* (:x11 a) (:y b)) (* (:x12 a) (:z b)) (:x13 a))"

  #_"(+ (* (:x20 a) (:x b)) (* (:x21 a) (:y b)) (* (:x22 a) (:z b)) (:x23 a))"
  (insta/transform to-prefix (insta/parse math
                                          "a.x00*a.x11*a.x22*a.x33 - a.x00*a.x11*a.x23*a.x32 +
         a.x00*a.x12*a.x23*a.x31 - a.x00*a.x12*a.x21*a.x33 +
         a.x00*a.x13*a.x21*a.x32 - a.x00*a.x13*a.x22*a.x31 -
         a.x01*a.x12*a.x23*a.x30 + a.x01*a.x12*a.x20*a.x33 -
         a.x01*a.x13*a.x20*a.x32 + a.x01*a.x13*a.x22*a.x30 -
         a.x01*a.x10*a.x22*a.x33 + a.x01*a.x10*a.x23*a.x32 +
         a.x02*a.x13*a.x20*a.x31 - a.x02*a.x13*a.x21*a.x30 +
         a.x02*a.x10*a.x21*a.x33 - a.x02*a.x10*a.x23*a.x31 +
         a.x02*a.x11*a.x23*a.x30 - a.x02*a.x11*a.x20*a.x33 -
         a.x03*a.x10*a.x21*a.x32 + a.x03*a.x10*a.x22*a.x31 -
         a.x03*a.x11*a.x22*a.x30 + a.x03*a.x11*a.x20*a.x32 -
         a.x03*a.x12*a.x20*a.x31 + a.x03*a.x12*a.x21*a.x30"))

  (- (* (:x00 a) (:x11 a) (:x22 a) (:x33 a))
     (+ (* (:x00 a) (:x11 a) (:x23 a) (:x32 a))
        (- (* (:x00 a) (:x12 a) (:x23 a) (:x31 a))
           (+ (* (:x00 a) (:x12 a) (:x21 a) (:x33 a))
              (- (* (:x00 a) (:x13 a) (:x21 a) (:x32 a))
                 (* (:x00 a) (:x13 a) (:x22 a) (:x31 a))))))

     (+ (* (:x01 a) (:x12 a) (:x23 a) (:x30 a))
        (- (* (:x01 a) (:x12 a) (:x20 a) (:x33 a))
           (+ (* (:x01 a) (:x13 a) (:x20 a) (:x32 a))
              (- (* (:x01 a) (:x13 a) (:x22 a) (:x30 a))
                 (+ (* (:x01 a) (:x10 a) (:x22 a) (:x33 a))
                    (* (:x01 a) (:x10 a) (:x23 a) (:x32 a))
                    (- (* (:x02 a) (:x13 a) (:x20 a) (:x31 a))
                       (+ (* (:x02 a) (:x13 a) (:x21 a) (:x30 a))
                          (- (* (:x02 a) (:x10 a) (:x21 a) (:x33 a))
                             (+ (* (:x02 a) (:x10 a) (:x23 a) (:x31 a))
                                (- (* (:x02 a) (:x11 a) (:x23 a) (:x30 a))
                                   (* (:x02 a) (:x11 a) (:x20 a) (:x33 a))))))))

                 (+ (* (:x03 a) (:x10 a) (:x21 a) (:x32 a))
                    (- (* (:x03 a) (:x10 a) (:x22 a) (:x31 a))
                       (+ (* (:x03 a) (:x11 a) (:x22 a) (:x30 a))
                          (- (* (:x03 a) (:x11 a) (:x20 a) (:x32 a))
                             (+ (* (:x03 a) (:x12 a) (:x20 a) (:x31 a))
                                (* (:x03 a) (:x12 a) (:x21 a) (:x30 a))))))))))))

  (map (fn [s]
         (insta/transform to-prefix (insta/parse math s)))
       ["a.x00 + b.x00"
        "a.x10 + b.x10"
        "a.x20 + b.x20"
        "a.x30 + b.x30"
        "a.x01 + b.x01"
        "a.x11 + b.x11"
        "a.x21 + b.x21"
        "a.x31 + b.x31"
        "a.x02 + b.x02"
        "a.x12 + b.x12"
        "a.x22 + b.x22"
        "a.x32 + b.x32"
        "a.x03 + b.x03"
        "a.x13 + b.x13"
        "a.x23 + b.x23"
        "a.x33 + b.x33"])

  ;;
  (map (fn [s]
         (insta/transform to-prefix (insta/parse math s)))
       ["a.x12*a.x23*a.x31 - a.x13*a.x22*a.x31 + a.x13*a.x21*a.x32 - a.x11*a.x23*a.x32 - a.x12*a.x21*a.x33 + a.x11*a.x22*a.x33"
        "a.x03*a.x22*a.x31 - a.x02*a.x23*a.x31 - a.x03*a.x21*a.x32 + a.x01*a.x23*a.x32 + a.x02*a.x21*a.x33 - a.x01*a.x22*a.x33"
        "a.x02*a.x13*a.x31 - a.x03*a.x12*a.x31 + a.x03*a.x11*a.x32 - a.x01*a.x13*a.x32 - a.x02*a.x11*a.x33 + a.x01*a.x12*a.x33"
        "a.x03*a.x12*a.x21 - a.x02*a.x13*a.x21 - a.x03*a.x11*a.x22 + a.x01*a.x13*a.x22 + a.x02*a.x11*a.x23 - a.x01*a.x12*a.x23"
        "a.x13*a.x22*a.x30 - a.x12*a.x23*a.x30 - a.x13*a.x20*a.x32 + a.x10*a.x23*a.x32 + a.x12*a.x20*a.x33 - a.x10*a.x22*a.x33"
        "a.x02*a.x23*a.x30 - a.x03*a.x22*a.x30 + a.x03*a.x20*a.x32 - a.x00*a.x23*a.x32 - a.x02*a.x20*a.x33 + a.x00*a.x22*a.x33"
        "a.x03*a.x12*a.x30 - a.x02*a.x13*a.x30 - a.x03*a.x10*a.x32 + a.x00*a.x13*a.x32 + a.x02*a.x10*a.x33 - a.x00*a.x12*a.x33"
        "a.x02*a.x13*a.x20 - a.x03*a.x12*a.x20 + a.x03*a.x10*a.x22 - a.x00*a.x13*a.x22 - a.x02*a.x10*a.x23 + a.x00*a.x12*a.x23"
        "a.x11*a.x23*a.x30 - a.x13*a.x21*a.x30 + a.x13*a.x20*a.x31 - a.x10*a.x23*a.x31 - a.x11*a.x20*a.x33 + a.x10*a.x21*a.x33"
        "a.x03*a.x21*a.x30 - a.x01*a.x23*a.x30 - a.x03*a.x20*a.x31 + a.x00*a.x23*a.x31 + a.x01*a.x20*a.x33 - a.x00*a.x21*a.x33"
        "a.x01*a.x13*a.x30 - a.x03*a.x11*a.x30 + a.x03*a.x10*a.x31 - a.x00*a.x13*a.x31 - a.x01*a.x10*a.x33 + a.x00*a.x11*a.x33"
        "a.x03*a.x11*a.x20 - a.x01*a.x13*a.x20 - a.x03*a.x10*a.x21 + a.x00*a.x13*a.x21 + a.x01*a.x10*a.x23 - a.x00*a.x11*a.x23"
        "a.x12*a.x21*a.x30 - a.x11*a.x22*a.x30 - a.x12*a.x20*a.x31 + a.x10*a.x22*a.x31 + a.x11*a.x20*a.x32 - a.x10*a.x21*a.x32"
        "a.x01*a.x22*a.x30 - a.x02*a.x21*a.x30 + a.x02*a.x20*a.x31 - a.x00*a.x22*a.x31 - a.x01*a.x20*a.x32 + a.x00*a.x21*a.x32"
        "a.x02*a.x11*a.x30 - a.x01*a.x12*a.x30 - a.x02*a.x10*a.x31 + a.x00*a.x12*a.x31 + a.x01*a.x10*a.x32 - a.x00*a.x11*a.x32"
        "a.x01*a.x12*a.x20 - a.x02*a.x11*a.x20 + a.x02*a.x10*a.x21 - a.x00*a.x12*a.x21 - a.x01*a.x10*a.x22 + a.x00*a.x11*a.x22"])

  )

