(ns clodiuno-debug.utils-test
  (:require [clodiuno.core :as core
             :refer [LOW HIGH INPUT OUTPUT ANALOG PWM SERVO]]
            [clodiuno-debug.utils :as sut]
            [clodiuno-debug.test-commons
             :refer [pin-mapping PIN0 PIN1 IO CLK CE]]
            [clodiuno-debug.test-helpers :refer [catch-all with-logged-event-maps]]
            [clojure.test :refer [deftest is]]
            [clodiuno-debug.firmata-debug
             :refer [HIGH_ARROW LOW_ARROW PCLK PCLK_ARROW NCLK NCLK_ARROW]]))

(def pin-mapping3
  (conj pin-mapping
        {:num 2 :color :green :name "PIN0"}
        {:num 3 :color :green :name "PIN1"}))

(def pin-mapping4
  (conj pin-mapping
        {:num 0 :color :green :name "PIN--0"}
        {:num 1 :color :green :name "PIN--1"}))

(def pin-mapping5
  [{:num 0 :color "green" :name "PIN--0"}
   {:num 1 :color :orange :name "PIN--1"}])

(deftest two-pins-same-name-test
  (is (= {:ex-msg "Assert failed: The pin PIN0 has been mapped several times. The pin PIN1 has been mapped several times"}
         (catch-all (sut/connect pin-mapping3
                                 :debug true
                                 :output-filename "output/nok-test.md")))))

(deftest two-pins-same-num-test
  (is (= {:ex-msg "Assert failed: Pin number 00 has been mapped several times. Pin number 01 has been mapped several times"}
         (catch-all (sut/connect pin-mapping4
                                 :debug true
                                 :output-filename "output/nok-test.md")))))

(deftest incorrect-pin-mapping-test
  (is (= {:ex-msg "Assert failed: One of the pins' color is not a keyword"}
         (catch-all (sut/connect pin-mapping5
                                 :debug true
                                 :output-filename "output/nok-test.md")))))

(deftest no-output-file-test
  (with-logged-event-maps logs
    (let [_board (sut/connect pin-mapping
                              :debug true
                              :output-filename nil)]
      (is (= [{:msg "No output file for this board in debug mode." :level :warn}]
             @logs)))))


(deftest export-signal-test
  (let [board (sut/connect pin-mapping :debug true :output-name "export-signal")]
    (core/pin-mode board PIN0 INPUT)
    (core/pin-mode board PIN1 PWM)
    (core/pin-mode board CLK OUTPUT)
    (core/pin-mode board IO ANALOG)
    (core/pin-mode board CE OUTPUT)

    (core/digital-read board PIN0)
    (core/digital-write board CLK HIGH)
    (core/digital-write board CLK LOW)
    (core/digital-write board CLK PCLK)
    (core/digital-write board CLK PCLK_ARROW)
    (core/digital-write board CLK LOW)
    (core/digital-write board CE LOW)
    (core/digital-write board CLK NCLK)
    (core/digital-write board CLK NCLK)
    (core/digital-write board CLK NCLK_ARROW)
    (core/digital-write board CE HIGH_ARROW)
    (core/digital-write board CE LOW_ARROW)
    (core/analog-read board IO)
    (core/analog-write board PIN1 42)
    (core/analog-write board PIN1 21)
    (core/analog-write board PIN1 23)
    (core/analog-write board PIN1 25)
    (core/analog-write board PIN1 27)
    (core/analog-write board PIN1 29)
    (core/analog-write board PIN1 31)
    (core/analog-write board PIN1 33)
    (core/analog-write board PIN1 35)
    (core/digital-write board CLK HIGH)

    (is (true? (sut/export-signal board)))
    (is (= (slurp "test-resources/export-signal.json")
           (slurp "output/export-signal.json")))))
