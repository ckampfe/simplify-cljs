(ns simplify.mesh)

(defrecord Mesh [triangles])

(defn new-mesh [triangles]
  (->Mesh triangles))
