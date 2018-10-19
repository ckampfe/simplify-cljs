(ns simplify.stl
  (:require [simplify.mesh :as mesh]
            [simplify.triangle :as triangle]
            [simplify.vector :as vector]))

(def TWO_BYTES (new js/ArrayBuffer 2))
(def TWO_BYTE_VIEW (new js/Uint8Array TWO_BYTES))
(def TWO_BYTES_AS_UINT16 (new js/Uint16Array TWO_BYTES))

(def FOUR_BYTES (new js/Uint8Array (new js/ArrayBuffer 4)))
(def FOUR_BYTE_DV (new js/DataView (.-buffer FOUR_BYTES)))

(def FOUR_BYTES_FOR_WRITE (new js/ArrayBuffer 4))
(def FOUR_BYTE_VIEW (new js/Uint8Array FOUR_BYTES_FOR_WRITE))
(def FOUR_BYTES_AS_INT32 (new js/Uint32Array FOUR_BYTES_FOR_WRITE))
(def FOUR_BYTES_AS_FLOAT32 (new js/Float32Array FOUR_BYTES_FOR_WRITE))

(defn int-to-2-bytes-unsigned-le [n]
  (aset TWO_BYTES_AS_UINT16 0 n)
  (array
   (aget TWO_BYTE_VIEW 0)
   (aget TWO_BYTE_VIEW 1)))

(defn int-to-4-bytes-unsigned-le [n]
  (aset FOUR_BYTES_AS_INT32 0 n)
  (array
   (aget FOUR_BYTE_VIEW 0)
   (aget FOUR_BYTE_VIEW 1)
   (aget FOUR_BYTE_VIEW 2)
   (aget FOUR_BYTE_VIEW 3)))

(defn signed-int-from-4-bytes [byte-array]
  (aset FOUR_BYTES 0 (aget byte-array 0))
  (aset FOUR_BYTES 1 (aget byte-array 1))
  (aset FOUR_BYTES 2 (aget byte-array 2))
  (aset FOUR_BYTES 3 (aget byte-array 3))
  (.getInt32 FOUR_BYTE_DV 0 true))

(defn unsigned-int-from-4-bytes-le [byte-array]
  (aset FOUR_BYTES 0 (aget byte-array 0))
  (aset FOUR_BYTES 1 (aget byte-array 1))
  (aset FOUR_BYTES 2 (aget byte-array 2))
  (aset FOUR_BYTES 3 (aget byte-array 3))
  (.getUint32 FOUR_BYTE_DV 0 true))

(defn float-to-4-bytes-unsigned-le [n]
  (aset FOUR_BYTES_AS_FLOAT32 0 n)
  (array
   (aget FOUR_BYTE_VIEW 0)
   (aget FOUR_BYTE_VIEW 1)
   (aget FOUR_BYTE_VIEW 2)
   (aget FOUR_BYTE_VIEW 3)))

;; Each triangle is described by twelve 32-bit floating-point numbers: three for the normal and then three for the X/Y/Z coordinate of each vertex – just as with the ASCII version of STL.
;; After these follows a 2-byte ("short") unsigned integer that is the "attribute byte count" – in the standard format, this should be zero because most software does not understand anything else.[6]

;; Floating-point numbers are represented as IEEE floating-point numbers and are assumed to be little-endian, although this is not stated in documentation. 
(defn thirty-two-bit-float-from-4-bytes-le [byte-array]
  (aset FOUR_BYTES 0 (aget byte-array 0))
  (aset FOUR_BYTES 1 (aget byte-array 1))
  (aset FOUR_BYTES 2 (aget byte-array 2))
  (aset FOUR_BYTES 3 (aget byte-array 3))
  (.getFloat32 FOUR_BYTE_DV 0 true))

(def attribute-byte-count-bytes (int-to-2-bytes-unsigned-le 0))

(defrecord State [bytes result position])

(defn take-byte! [state]
  (let [current-position (.-position state)]
    (set! (.-position state)
          (inc current-position))
    (aget (.-bytes state) current-position)))

(defn take-bytes! [n state]
  (let [bytes (make-array n)]
    (dotimes [i n]
      (aset bytes i (take-byte! state)))
    bytes))

(defn get-three-floats! [state]
  (vector/->Vector
   (thirty-two-bit-float-from-4-bytes-le (take-bytes! 4 state))
   (thirty-two-bit-float-from-4-bytes-le (take-bytes! 4 state))
   (thirty-two-bit-float-from-4-bytes-le (take-bytes! 4 state))))

(defn get-triangle! [state]
  (let [_nvector (get-three-floats! state)
        x (get-three-floats! state)
        y (get-three-floats! state)
        z (get-three-floats! state)
        _attribute-byte-count (take-bytes! 2 state)]
    (triangle/->Triangle x y z)))

;; https://en.wikipedia.org/wiki/STL_(file_format)#Binary_STL
(defn load-binary-stl [bytes]
  (let [state (State. bytes [] 0)
        header (take-bytes! 80 state)
        number-of-triangles (unsigned-int-from-4-bytes-le (take-bytes! 4 state))
        triangles (map (fn [_]
                         (let [tri (get-triangle! state)]
                           #_(println tri)
                           tri))
                       (range number-of-triangles))]

    (mesh/new-mesh triangles)))

(defn get-triangle-bytes [t]
  (let [normal (triangle/normal t)
        normal-vector-bytes (->> ((juxt :x :y :z) normal)
                                 (map float-to-4-bytes-unsigned-le)
                                 (reduce (fn [bytes float-bytes]
                                           (.concat bytes float-bytes))))

        vertex1-bytes (->> [:x :y :z]
                           (map #(get-in t [:v1 %]))
                           (map float-to-4-bytes-unsigned-le)
                           (reduce (fn [bytes float-bytes]
                                     (.concat bytes float-bytes))))

        vertex2-bytes (->> [:x :y :z]
                           (map #(get-in t [:v2 %]))
                           (map float-to-4-bytes-unsigned-le)
                           (reduce (fn [bytes float-bytes]
                                     (.concat bytes float-bytes))))

        vertex3-bytes (->> [:x :y :z]
                           (map #(get-in t [:v3 %]))
                           (map float-to-4-bytes-unsigned-le)
                           (reduce (fn [bytes float-bytes]
                                     (.concat bytes float-bytes))))]

    (.concat normal-vector-bytes
             vertex1-bytes
             vertex2-bytes
             vertex3-bytes
             attribute-byte-count-bytes)))

(defn save-binary-stl [mesh]
  (let [eighty-bytes (into-array (repeat 80 0))
        triangles (:triangles mesh)
        x (println (take 5 triangles))
        number-of-triangle-bytes (->> triangles
                                      count
                                      int-to-4-bytes-unsigned-le)
        triangle-bytes (reduce (fn [acc t] (.concat acc (get-triangle-bytes t)))
                               (array)
                               triangles)
        combined (.concat eighty-bytes
                          number-of-triangle-bytes
                          triangle-bytes)]

    (println "number of triangle bytes:" number-of-triangle-bytes)

    (.from js/Uint8Array combined)))

(defn roundtrip [bytes]
  (let [mesh1 (load-binary-stl bytes)
        saved (save-binary-stl mesh1)
        mesh2 (load-binary-stl saved)]
    (println "byte length of input:" (.-length bytes))
    (println "byte length of saved:" (.-length saved))
    (println "count of initial:" (count (:triangles mesh1)))
    (println "count of rountripped:" (count (:triangles mesh2)))
    (println "(= mesh1 mesh)" (= mesh1 mesh2))))
