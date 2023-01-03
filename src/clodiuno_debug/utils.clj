(ns clodiuno-debug.utils
  "╭───────────────────────────╮
   │ Arduino utility functions │
   ╰───────────────────────────╯"
  (:require [cheshire.core :refer [generate-stream]]
            [clodiuno.core :as core :refer [LOW HIGH OUTPUT]]
            [clodiuno.firmata]
            [clodiuno-debug.firmata-debug :as debug]
            [clodiuno-debug.timbre-config]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [taoensso.timbre :as log])
  (:import (gnu.io CommPortIdentifier)))

;; Utility functions
;; -----------------

(defn connect
  "option :baudrate 9600 doesn't work.
   What you'll get when using the 'debug' mode:

   ```clojure
   TODO
   ```"
  [& {:as opts
      :keys [port debug]
      :or {debug false}}]
  (if debug
    (core/arduino :firmata-debug opts)
    (do
      (log/infof "Connecting to Arduino on port %s" port)
      (core/arduino :firmata port))))

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
    (empty? (->> (format "/usr/local/bin/wavedrom-cli --input %s --png %s" out_signal out_png)
                 (shell/sh "/bin/zsh"
                           "-c")
                 :out))))

(defn close-board [board]
  (core/close board))

(defn list-ports
  "List available communication ports. Do not connect to them."
  []
  (let [ports (CommPortIdentifier/getPortIdentifiers)]
    (if (.hasMoreElements ports)
      (into []
            (for [port (enumeration-seq ports)
                  :let [name (.getName port)]]
              name))
      [])))

(defn integrated-led-blink [board]
  (println "blinking led 13 -")
  (core/pin-mode board 13 OUTPUT)
  (core/digital-write board 13 HIGH)
  (Thread/sleep 1000)
  (core/digital-write board 13 LOW))

(defn impulse
  "Sends an impulse on the pin."
  [board pin & {:keys [wait] :or {wait 100}}]
  (let [{:keys [mode]} (debug/pin-info board pin)]
    (core/pin-mode board pin OUTPUT)
    (core/digital-write board pin HIGH)
    ;; TODO write a sleep function that doesn't wait if the board is of type debug!
    (Thread/sleep wait)
    (core/digital-write board pin LOW)
    ;; restore mode
    (core/pin-mode board pin mode)))