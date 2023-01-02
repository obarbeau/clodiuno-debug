(ns clodiuno-debug.utils
  "╭───────────────────────────╮
   │ Arduino utility functions │
   ╰───────────────────────────╯"
  (:require [cheshire.core :refer [generate-stream]]
            [clodiuno.core :as core :refer [LOW HIGH OUTPUT]]
            [clodiuno.firmata]
            [clodiuno-debug.firmata-debug :as debug]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import (gnu.io CommPortIdentifier)))

;; Utility functions
;; -----------------

(defn connect
  "option :baudrate 9600 doesn't work.
   What you'll get:

```clojure
   #<Ref@6a24f583:
   {:port #object[gnu.io.RXTXPort 0x25ebea2b \"/dev/ttyACM0\"],
    :interface :firmata,
    :version [2 5],
    :firmware {:version [2 5], :name \"S t a n d a r d F i r m a t a . i n o \"},
    :pin-mapping
    [{:num 38, :color \"blanc\", :name \"CLK\", :ansi :white}
     {:num 39, :color \"jaune\", :name \"IO\", :ansi :yellow}
     {:num 40, :color \"orange\", :name \"CE/_RST\", :ansi nil}],
    :digital-out
    {0 (0 0 0 0 0 0 0 0),
     1 (0 0 0 0 0 0 0 0),
     2 (0 0 0 0 0 0 0 0),
     3 (0 0 0 0 0 0 0 0),
     4 (0 0 0 0 0 0 0 0),
     5 (0 0 0 0 0 0 0 0),
     6 (0 0 0 0 0 0 0 0)},
    :digital-in
    {0 (0 0 0 0 0 0 0 0),
     1 (0 0 0 0 0 0 0 0),
     2 (0 0 0 0 0 0 0 0),
     3 (0 0 0 0 0 0 0 0),
     4 (0 0 0 0 0 0 0 0),
     5 (0 0 0 0 0 0 0 0),
     6 (0 0 0 0 0 0 0 0)},
    :i2c {:last-blocking-read nil},
    :callbacks {:msg nil}}>
```"
  [pin-mapping & {:keys [debug output-dir output-name port]
                  :or {debug false
                       output-dir "./output"
                       port "/dev/ttyACM0"}}]
  (let [same-name (->> pin-mapping
                       (map :name)
                       (frequencies)
                       (filter #(< 1 (val %))))
        same-num (->> pin-mapping
                      (map :num)
                      (frequencies)
                      (filter #(< 1 (val %))))]
    (assert (empty? same-name)
            (->> same-name
                 (map first)
                 (map #(format "The pin %s has been mapped several times" %))
                 (str/join ". ")))
    (assert (empty? same-num)
            (->> same-num
                 (map first)
                 (map #(format "Pin number %02d has been mapped several times" %))
                 (str/join ". "))))
  (if debug
    (core/arduino :firmata-debug output-dir output-name pin-mapping)
    (do
      (log/infof "Connecting to Arduino on port %s" port)
      (let [board (core/arduino :firmata port)]
        (#'clodiuno.firmata/assoc-in! board [:pin-mapping] pin-mapping)
        board))))

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