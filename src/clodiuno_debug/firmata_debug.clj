(ns clodiuno-debug.firmata-debug
  "╭──────────────────────────────╮
   │ Firmata protocol, debug mode │
   ╰──────────────────────────────╯"
  (:require [clodiuno.core :as ccore
             :refer [LOW HIGH INPUT OUTPUT ANALOG PWM SERVO]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [io.aviso.ansi :as ansi]
            [taoensso.timbre :as log]))

(def REPORT-ANALOG "enable analog input by pin #"  0xC0)
(def REPORT-DIGITAL "enable digital input by port" 0xD0)
(def arduino-port-count "eight pins per port" 7)

(def available-colors
  "background color / preferred foreground color"
  {:black :white
   :red :white
   :white :black
   :yellow :black
   :green :black
   :blue :black
   :cyan :black
   :magenta :white})

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defprotocol IOutputStream
  (getOutputStream [this]
    "Returns a (buffered)output stream.")
  (close [this]
    "Closes the stream"))

;; private functions
;; -----------------

(defn- assoc-in! [r ks v]
  (dosync (alter r assoc-in ks v)))

(defn- write-os [os msg]
  (doseq [b (.getBytes msg)]
    (.write os (int b)))
  (.flush os))

(defn- write-bytes
  "Writes the message to the output-stream if it exists.
   Returns the message."
  [board msg]
  (when-let [os (:output-stream @board)]
    (write-os (.getOutputStream os) msg))
  msg)

(defn- get-aviso-color [k type]
  (or (get (ns-interns 'io.aviso.ansi)
           (symbol (str (symbol k) type)))
      identity))

(defn- get-color-bg [k]
  (get-aviso-color k "-bg"))

(defn- get-color [k]
  (get-aviso-color k ""))

(defn- colored-pin [num color]
  ((get-color-bg color)
   ((get-color (get available-colors color :no-color)) (format "%02d" num))))

(defn- check-mapping
  "Pin color is not mandatory but if present, must be a keyword
   TODO: check only allowed keywords"
  [pin-mapping]
  (assert (->> pin-mapping
               (map :color)
               (every? #(or (keyword? %) (nil? %))))
          "One of the pins' color is not a keyword")
  pin-mapping)

(defn- pin-action [board {:keys [num name color] :as _pin-info} action & args]
  (write-bytes board (format
                      "| %s | %s | %s | %s |\n"
                      (colored-pin num color)
                      name
                      action
                      (str/join " " args))))

(defn- update-wave
  [board fn-wave & {:keys [fn-data]}]
  (assoc-in! board [:signal]
             (for [{:keys [num] :as sig} (:signal @board)]
               (cond-> sig
                 true (update :wave #(str % (fn-wave num)))
                 fn-data (update :data #(fn-data % num))))))

;; Utility functions
;; -----------------

(defn init-signal
  "Returns a map of
   ```clojure
   {0 []
    1 []
    ,,,}
   ```"
  [pin-mapping]
  (->> pin-mapping
       (map #(dissoc % :color))
       (map #(assoc % :wave "z" :data []))))

(defn pins
  "Returns a map of all pins indexed by their num:
   ```clojure
   {0 {:num 0 :color :black :name \"PIN0\"}
    1 {:num 1 :color :red :name \"PIN1\"}
    ,,,}
   ```"
  [pin-mapping]
  (->> pin-mapping
       (map-indexed (fn [_ item] [(:num item) item]))
       (into {})))

(defn pin-info
  "Merges the pin mapping with pin port, mode & the mode of pins on same port.
   For example:
   ```clojure
   {:num 0 :color :black :name \"PIN0\" :port 0 :mode 'INPUT
    :pin-modes-on-port '(INPUT PWM nil nil nil nil nil nil)}
   ```"
  [conn pin]
  (let [full-pin (-> (:pin-mapping @conn) pins (get pin))
        port (int (/ pin 8))
        pin-modes-on-port (get (@conn :pin-mode) port)
        mode (->> pin-modes-on-port (drop (mod pin 8)) first)]
    (assert full-pin (format "Pin %d has not been mapped." pin))
    (merge {:mode mode :pin-modes-on-port pin-modes-on-port :port port}
           full-pin)))

;; Firmata protocol implementation
;; -------------------------------

(defmethod ccore/arduino :firmata-debug
  [type output-dir output-name pin-mapping]
  (ref {:interface type
        :output-dir output-dir
        :output-name output-name
        :output-stream (if output-name
                         (let [filename (format "%s/%s.md" output-dir output-name)
                               _ (io/make-parents filename)
                               bof (io/make-output-stream filename {})]
                           (write-os bof "| pin | name | action | args |\n")
                           (write-os bof "| --: | --: | --: | --: |\n")
                           (reify
                             IOutputStream
                             (getOutputStream [_]
                               bof)
                             (close [_]
                               (.close bof))))
                         (log/warn "No output file for this board in debug mode."))
        :pin-mapping (check-mapping pin-mapping)
        :digital-out (into {}
                           (for [i (range 0 arduino-port-count)]
                             [i (repeat 8 0)]))
        :pin-mode (into {}
                        (for [i (range 0 arduino-port-count)]
                          [i (repeat 8 nil)]))
        :signal (init-signal pin-mapping)}))

(defmethod ccore/close :firmata-debug [board]
  (when-let [os (:output-stream @board)]
    (.close os)))

(defmethod ccore/digital-read :firmata-debug
  [board pin]
  (let [{:keys [mode] :as pin-info} (pin-info board pin)]
    (assert (= INPUT mode) (format "Pin %d is not in INPUT mode." pin))
    (pin-action board pin-info "digital-read")))

;; First check that the pin is in OUTPUT mode.
;; Change the :digital-out of the board for this pin.
;; Update the wave
(defmethod ccore/digital-write :firmata-debug [board pin value]
  (assert (contains? #{HIGH LOW} value)
          "Incorrect value to write on digital pin.")
  (let [{:keys [name mode port] :as pin-info} (pin-info board pin)
        digital-out-port (get-in @board [:digital-out port])
        beg (take (mod pin 8) digital-out-port)
        end (drop (inc (mod pin 8)) digital-out-port)
        state (concat beg [value] end)
        _ (println digital-out-port)
        current-value (nth digital-out-port (mod (inc pin) 8))
        high? (= HIGH value)]
    (println "current-value" pin current-value)
    (assert (= OUTPUT mode)
            (format "Pin %d (%s) is not in OUTPUT mode." pin name))
    (assoc-in! board [:digital-out port] state)
    (update-wave board #(if (= % pin)
                          (if high? "h" "l")
                          "."))
    (pin-action
     board
     pin-info
     "digital-write"
     (if high?
       (ansi/green-bg " HIGH ")
       (ansi/black-bg " LOW ")))))

;; pins with analog capability default to analog input
;; otherwise, pins default to digital output
(defmethod ccore/pin-mode :firmata-debug [board pin new-mode]
  (let [{:keys [pin-modes-on-port mode] :as pin-info} (pin-info board pin)
        mode-sym (condp = new-mode
                   INPUT 'INPUT
                   OUTPUT 'OUTPUT
                   ANALOG 'ANALOG
                   PWM 'PWM
                   SERVO 'SERVO)
        port (int (/ pin 8))
        beg (take (mod pin 8) pin-modes-on-port)
        end (drop (inc (mod pin 8)) pin-modes-on-port)
        state (concat beg [new-mode] end)]
    (when-not (= new-mode mode)
      (assoc-in! board [:pin-mode port] state)
      (pin-action board pin-info "position in mode" mode-sym))))

(defmethod ccore/analog-read :firmata-debug [board pin]
  (let [{:keys [name mode] :as pin-info} (pin-info board pin)
        value (get (@board :analog) pin)]
    (assert (= ANALOG mode)
            (format "Pin %d (%s) is not in ANALOG mode." pin name))
    (pin-action board pin-info "analog read" value)))

(defmethod ccore/analog-write :firmata-debug [board pin val]
  (let [{:keys [name mode] :as pin-info} (pin-info board pin)]
    (assert (= PWM mode) (format "Pin %d (%s) is not in PWM mode." pin name))
    (assert (<= 0 val 255)
            (format "PWM value should be between 0 and 255: %d" val))
    (update-wave board
                 #(if (= % pin)
                    "="
                    ".")
                 :fn-data (fn [data p]
                            (if (= p pin)
                              (conj data (str val))
                              data)))
    (pin-action board pin-info "analog write" val)))

(defn- change-pin-state [board type pin enabled?]
  (let [{:keys [name mode] :as pin-info} (pin-info board pin)
        p-pin-action (partial pin-action
                              board
                              pin-info
                              (if enabled? "enable" "disable")
                              "with code")
        state (if enabled? "enabled" "disabled")]
    (cond (= type :analog)
          (do
            (assert (some #{ANALOG PWM SERVO} [mode])
                    (format "Pin %d (%s) should be in ANALOG, PWM or SERVO mode if %s as :analog." pin name state))
            (p-pin-action
             (bit-or REPORT-ANALOG pin)
             (if enabled? 1 0)))
          (= type :digital)
          (do
            (assert (some #{INPUT OUTPUT} [mode])
                    (format "Pin %d (%s) should be in ANALOG or PWM mode if %s as :analog." pin name state))
            (p-pin-action
             (bit-or REPORT-DIGITAL (int (/ pin 8)))
             (if enabled? 1 0)))
          :else (throw (ex-info "Unknown pin type." {})))))

(defmethod ccore/enable-pin :firmata-debug [board type pin]
  (change-pin-state board type pin true))

(defmethod ccore/disable-pin :firmata-debug [board type pin]
  (change-pin-state board type pin false))
