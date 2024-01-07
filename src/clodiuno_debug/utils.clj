(ns clodiuno-debug.utils
  "╭───────────────────────────╮
   │ Arduino utility functions │
   ╰───────────────────────────╯"
  (:require [cheshire.core :refer [generate-stream]]
            [clodiuno-debug.timbre-config]
            [clojure.java.io :as io]
            [org.httpkit.client :as http])
  (:import (gnu.io CommPortIdentifier)))

;; Utility functions
;; -----------------

(def
  ^{:arglists '([map f? key val] [map f? key val & kvs])
    :doc "assoc[iate] if (f? val) is true. When applied to a map, returns a new
    map of the same (hashed/sorted) type, that contains the mapping of key(s) to
    val(s) that are true for f?. When applied to a vector, returns a new vector
    that contains val at index. Note - index must be <= (count vector).
    From `iroh-core.core`"
    :static true}
  assoc-if
  (fn ^:static assoc-if
    ([map f? key val]
     (if (f? val)
       (assoc map key val)
       map))
    ([map f? key val & kvs]
     (let [ret (if (f? val)
                 (assoc map key val)
                 map)]
       (if kvs
         (if (next kvs)
           (recur ret f? (first kvs) (second kvs) (nnext kvs))
           (throw (IllegalArgumentException.
                   "assoc-if expects even number of arguments after map/vector, found odd number")))
         ret)))))

(defn assoc-some?
  "like assoc but for non-nil values. From `iroh-core.core`."
  [m & args]
  (apply assoc-if m some? args))

(defn list-ports
  "List available communication ports. Do not connect to them."
  []
  (let [ports (CommPortIdentifier/getPortIdentifiers)]
    (if (.hasMoreElements ports)
      (into []
            (for [port (enumeration-seq ports)
                  :let [name (.getName ^CommPortIdentifier port)]]
              name))
      [])))

(defn export-signal [board]
  (let [out_signal (format "%s/%s.json" (:output-dir @board) (:output-name @board))
        out_png (format "%s/%s.png" (:output-dir @board) (:output-name @board))]
    (generate-stream {:signal (->> (:signal @board)
                                   (map #(dissoc % :num)))
                      :config {:hscale 1
                               :skin "default" #_"narrow"}
                      :head {:text "WaveDrom example"
                             :tick 0
                             :every 2}
                      :foot {:text "Figure 100"
                             :tock 9}}
                     (clojure.java.io/writer out_signal)
                     {:pretty true})
    (let [{:keys [body status]} @(http/post "http://localhost:8000/wavedrom"
                                            {:as :stream
                                             :headers {"Accept" "image/svg+xml"}
                                             :body (clojure.java.io/input-stream out_signal)})]
      (spit out_png body)
      (= 200 status))))


