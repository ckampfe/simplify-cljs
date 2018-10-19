(ns simplify.face)

(defn degenerate [face]
  (let [v1 (-> f :v1 :vector)
        v2 (-> f :v2 :vector)
        v3 (-> f :v3 :vector)]
    (or (= v1 v2)
        (= v1 v3)
        (= v2 v3))))

(defn normal [face]
  (let [e1 (vector/sub (:vector (:v2 f))
                       (:vector (:v1 f)))
        e2 (vector/sub (:vector (:v3 f))
                       (:vector (:v1 f)))]

    (-> (vector/cross e1 e2)
        vector/normalize)))
