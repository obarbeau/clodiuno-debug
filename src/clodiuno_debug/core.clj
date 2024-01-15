(ns clodiuno-debug.core
  "╭────────────────────╮
   │ Arduino debug core │
   ╰────────────────────╯"
  (:require [clodiuno.core :as ccore :refer [LOW HIGH OUTPUT]]
            [clodiuno.firmata]
            [clodiuno-debug.firmata-debug :as debug]
            [clodiuno-debug.timbre-config]
            [clodiuno-debug.utils :as utils]
            [taoensso.timbre :as log]))

(def default-arduino-port "/dev/ttyACM0")

(defn connect
  "option :baudrate 9600 doesn't work.
   port required only if a real arduino is wrapped.
   What you'll get when using the 'debug' mode:

   ```clojure
   TODO
   ```"
  [& {:as opts
      :keys [port debug]
      :or {debug false}}]
  (when port
    (let [port-index (.indexOf ^clojure.lang.PersistentVector (utils/list-ports) port)
          port-exists? (pos? port-index)]
      (assert port-exists? (format "The port %s is not available." port))))
  (if debug
    (ccore/arduino :firmata-debug opts)
    (do
      (log/infof "Connecting to Arduino on port %s" port)
      (ccore/arduino :firmata port))))

(defn close-board [board]
  (ccore/close board))

(defn integrated-led-blink [board]
  (println "blinking led 13 -")
  (ccore/pin-mode board 13 OUTPUT)
  (ccore/digital-write board 13 HIGH)
  (Thread/sleep 1000)
  (ccore/digital-write board 13 LOW))

(defn impulse
  "Sends an impulse on the pin."
  [board pin & {:keys [wait] :or {wait 100}}]
  (let [{:keys [mode]} (debug/pin-info board pin)]
    (ccore/pin-mode board pin OUTPUT)
    (ccore/digital-write board pin HIGH)
    ;; TODO write a sleep function that doesn't wait if the board is of type debug!
    (Thread/sleep wait)
    (ccore/digital-write board pin LOW)
    ;; restore mode
    (ccore/pin-mode board pin mode))) "