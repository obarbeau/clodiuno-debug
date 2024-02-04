(ns clodiuno-debug.utils-test
  (:require [clodiuno.core :as ccore
             :refer [LOW HIGH INPUT OUTPUT ANALOG PWM SERVO]]
            [clodiuno-debug.core :as debug-core]
            [clodiuno-debug.utils :as sut]
            [clodiuno-debug.test-commons
             :refer [pin-mapping PIN0 PIN1 IO CLK CE]]
            [clodiuno-debug.test-helpers :refer [with-logged-event-maps]]
            [clojure.test :refer [deftest is]]
            [clodiuno-debug.firmata-debug
             :refer [HIGH_ARROW LOW_ARROW PCLK PCLK_ARROW NCLK NCLK_ARROW]]))

(deftest no-output-file-test
  (with-logged-event-maps logs
    (let [_board (debug-core/connect :pin-mapping pin-mapping
                                     :debug true
                                     :output-filename nil)]
      (is (= [{:msg "No output file for this board in debug mode." :level :warn}]
             @logs)))))

(deftest export-signal-test
  (let [board (debug-core/connect :pin-mapping pin-mapping
                                  :debug true
                                  :output-name "export-signal")]
    (ccore/pin-mode board PIN0 INPUT)
    (ccore/pin-mode board PIN1 PWM)
    (ccore/pin-mode board CLK OUTPUT)
    (ccore/pin-mode board IO ANALOG)
    (ccore/pin-mode board CE OUTPUT)

    (ccore/digital-read board PIN0)
    (ccore/digital-write board CLK HIGH)
    (ccore/digital-write board CLK LOW)
    (ccore/digital-write board CLK PCLK)
    (ccore/digital-write board CLK PCLK_ARROW)
    (ccore/digital-write board CLK LOW)
    (ccore/digital-write board CE LOW)
    (ccore/digital-write board CLK NCLK)
    (ccore/digital-write board CLK NCLK)
    (ccore/digital-write board CLK NCLK_ARROW)
    (ccore/digital-write board CE HIGH_ARROW)
    (ccore/digital-write board CE LOW_ARROW)
    (ccore/analog-read board IO)
    (ccore/analog-write board PIN1 42)
    (ccore/analog-write board PIN1 21)
    (ccore/analog-write board PIN1 23)
    (ccore/analog-write board PIN1 25)
    (ccore/analog-write board PIN1 27)
    (ccore/analog-write board PIN1 29)
    (ccore/analog-write board PIN1 31)
    (ccore/analog-write board PIN1 33)
    (ccore/analog-write board PIN1 35)
    (ccore/digital-write board CLK HIGH)

    (is (true? (sut/export-signal board)))
    (is (= (slurp "test-resources/export-signal.json")
           (slurp "output/export-signal.json")))))
