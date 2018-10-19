(ns simplify.main
  (:require [simplify.stl :as stl]))

;; var oReq = new XMLHttpRequest();
;; oReq.open("GET", "/myfile.png", true);
;; oReq.responseType = "arraybuffer";
;;
;; oReq.onload = function (oEvent) {
;;   var arrayBuffer = oReq.response; // Note: not oReq.responseText
;;   if (arrayBuffer) {
;;   var byteArray = new Uint8Array(arrayBuffer);
;;   for (var i = 0; i < byteArray.byteLength; i++) {
;;   // do something with each byte in the array
;;   }
;;   }
;; };
;; oReq.send(null);

;; via:
;; https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/Sending_and_Receiving_Binary_Data
(defn do-it []
  (.log js/console "starting req")
  (let [oreq (js/XMLHttpRequest.)]
    (.open oreq "GET" "http://localhost:8000/pizero-scase.stl" true)
    (set! (.-responseType oreq) "arraybuffer")

    (set! (.-onload oreq)
          (fn [oevent]
            (let [array-buffer (.-response oreq)]
              (if array-buffer
                (let [byte-array (js/Uint8Array. array-buffer)]
                  (.log js/console ">>>>>>>>>>>>>>>")
                  #_(.log js/console (stl/load-binary-stl byte-array))
                  (stl/roundtrip byte-array)
                  #_(.log js/console array-buffer)
                  (.log js/console "<<<<<<<<<<<<<<<")
                  )
                (.log js/console "request not successful")))))
    (.send oreq nil)))
