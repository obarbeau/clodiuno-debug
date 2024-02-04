(ns clodiuno-debug.firmata-debug-test
  (:require [clodiuno.core :as ccore
             :refer [LOW HIGH INPUT OUTPUT ANALOG PWM SERVO]]
            [clodiuno-debug.core :as debug-core]
            [clodiuno-debug.firmata-debug :as sut]
            [clodiuno-debug.test-commons
             :refer [pin-mapping PIN0 PIN1 IO CLK CE NOT-A-PIN output-name]]
            [clodiuno-debug.test-helpers :refer [catch-all delete-output-dir]]
            [clojure.test :refer [deftest is testing]]))

;; init
(def pin-mapping2
  (conj pin-mapping
        {:num 2 :color :green   :name "PIN2"}
        {:num 3 :color :blue    :name "PIN3"}
        {:num 4 :color :cyan    :name "PIN4"}
        {:num 5 :color :magenta :name "PIN5"}))
(delete-output-dir)

;; tests
(deftest all-test
  (let [board (debug-core/connect :pin-mapping pin-mapping
                                  :debug true
                                  :output-name output-name)]
    (is (= :firmata-debug
           (:interface @board)))

    (testing "pin-mode"
      (is (= {:ex-msg "Assert failed: Pin 666 has not been mapped."}
             (catch-all (ccore/pin-mode board NOT-A-PIN INPUT))))
      (is (= "| [40m[37m00[m[m | PIN0 | position in mode | INPUT |\n"
             (ccore/pin-mode board PIN0 INPUT)))
      (is (= {0 [INPUT nil nil nil nil nil nil nil]
              1 [nil nil nil nil nil nil nil nil]
              2 [nil nil nil nil nil nil nil nil]
              3 [nil nil nil nil nil nil nil nil]
              4 [nil nil nil nil nil nil nil nil]
              5 [nil nil nil nil nil nil nil nil]
              6 [nil nil nil nil nil nil nil nil]}
             (:pin-mode @board)))
      (is (= "| [41m[37m01[m[m | PIN1 | position in mode | PWM |\n"
             (ccore/pin-mode board PIN1 PWM)))
      (is (= {0 [INPUT PWM nil nil nil nil nil nil]
              1 [nil nil nil nil nil nil nil nil]
              2 [nil nil nil nil nil nil nil nil]
              3 [nil nil nil nil nil nil nil nil]
              4 [nil nil nil nil nil nil nil nil]
              5 [nil nil nil nil nil nil nil nil]
              6 [nil nil nil nil nil nil nil nil]}
             (:pin-mode @board)))
      (is (= "| [47m[30m38[m[m | CLK | position in mode | OUTPUT |\n"
             (ccore/pin-mode board CLK OUTPUT)))
      (is (= {0 [INPUT PWM nil nil nil nil nil nil]
              1 [nil nil nil nil nil nil nil nil]
              2 [nil nil nil nil nil nil nil nil]
              3 [nil nil nil nil nil nil nil nil]
              4 [nil nil nil nil nil nil OUTPUT nil]
              5 [nil nil nil nil nil nil nil nil]
              6 [nil nil nil nil nil nil nil nil]}
             (:pin-mode @board)))
      (is (= "| [43m[30m39[m[m | IO | position in mode | ANALOG |\n"
             (ccore/pin-mode board IO ANALOG)))
      (is (= {0 [INPUT PWM nil nil nil nil nil nil]
              1 [nil nil nil nil nil nil nil nil]
              2 [nil nil nil nil nil nil nil nil]
              3 [nil nil nil nil nil nil nil nil]
              4 [nil nil nil nil nil nil OUTPUT ANALOG]
              5 [nil nil nil nil nil nil nil nil]
              6 [nil nil nil nil nil nil nil nil]}
             (:pin-mode @board)))
      (is (= "| [45m[37m40[m[m | CE/_RST | position in mode | SERVO |\n"
             (ccore/pin-mode board CE SERVO)))
      (is (= {0 [INPUT PWM nil nil nil nil nil nil]
              1 [nil nil nil nil nil nil nil nil]
              2 [nil nil nil nil nil nil nil nil]
              3 [nil nil nil nil nil nil nil nil]
              4 [nil nil nil nil nil nil OUTPUT ANALOG]
              5 [SERVO nil nil nil nil nil nil nil]
              6 [nil nil nil nil nil nil nil nil]}
             (:pin-mode @board))))

    (testing "digital-read"
      (is (= {:ex-msg "Assert failed: Pin 666 has not been mapped."}
             (catch-all (ccore/digital-read board NOT-A-PIN))))
      (is (= {:ex-msg "Assert failed: Pin 1 is not in INPUT mode."}
             (catch-all (ccore/digital-read board PIN1))))
      (is (= {:ex-msg "Assert failed: Pin 38 is not in INPUT mode."}
             (catch-all (ccore/digital-read board CLK))))
      (is (= {:ex-msg "Assert failed: Pin 39 is not in INPUT mode."}
             (catch-all (ccore/digital-read board IO))))
      (is (= {:ex-msg "Assert failed: Pin 40 is not in INPUT mode."}
             (catch-all (ccore/digital-read board CE))))
      (is (= "| [40m[37m00[m[m | PIN0 | digital-read |  |\n"
             (ccore/digital-read board PIN0))))

    (testing "digital-write"
      (is (= {:ex-msg "Assert failed: Pin 666 has not been mapped."}
             (catch-all (ccore/digital-write board NOT-A-PIN HIGH))))
      (is (= {:ex-msg "Assert failed: Pin 1 (PIN1) is not in OUTPUT mode."}
             (catch-all (ccore/digital-write board PIN1 HIGH))))
      (is (= {:ex-msg "Assert failed: Pin 0 (PIN0) is not in OUTPUT mode."}
             (catch-all (ccore/digital-write board PIN0 HIGH))))
      (is (= {:ex-msg "Assert failed: Pin 39 (IO) is not in OUTPUT mode."}
             (catch-all (ccore/digital-write board IO HIGH))))
      (is (= {:ex-msg "Assert failed: Pin 40 (CE/_RST) is not in OUTPUT mode."}
             (catch-all (ccore/digital-write board CE HIGH))))
      (is (= {:ex-msg "Assert failed: Incorrect value to write on digital pin."}
             (catch-all (ccore/digital-write board CLK 2))))
      (is (= [{:data [], :name "PIN0", :num 0, :wave "x"}
              {:data [], :name "PIN1", :num 1, :wave "x"}
              {:data [], :name "CLK", :num 38, :wave "x"}
              {:data [], :name "IO", :num 39, :wave "x"}
              {:data [], :name "CE/_RST", :num 40, :wave "x"}]
             (:signal @board)))
      (is (= "| [47m[30m38[m[m | CLK | digital-write | [42m HIGH [m |\n"
             (ccore/digital-write board CLK HIGH))
          "should write HIGH")
      (is (= {0 '(0 0 0 0 0 0 0 0)
              1 '(0 0 0 0 0 0 0 0)
              2 '(0 0 0 0 0 0 0 0)
              3 '(0 0 0 0 0 0 0 0)
              4 '(0 0 0 0 0 0 1 0)
              5 '(0 0 0 0 0 0 0 0)
              6 '(0 0 0 0 0 0 0 0)}
             (:digital-out @board))
          "should update the board")
      (is (= [{:data [], :name "PIN0", :num 0, :wave "x."}
              {:data [], :name "PIN1", :num 1, :wave "x."}
              {:data [], :name "CLK", :num 38, :wave "xh"}
              {:data [], :name "IO", :num 39, :wave "x."}
              {:data [], :name "CE/_RST", :num 40, :wave "x."}]
             (:signal @board))
          "should update the signal")
      (is (= "| [47m[30m38[m[m | CLK | digital-write | [40m LOW [m |\n"
             (ccore/digital-write board CLK LOW)))
      (is (= {0 '(0 0 0 0 0 0 0 0)
              1 '(0 0 0 0 0 0 0 0)
              2 '(0 0 0 0 0 0 0 0)
              3 '(0 0 0 0 0 0 0 0)
              4 '(0 0 0 0 0 0 0 0)
              5 '(0 0 0 0 0 0 0 0)
              6 '(0 0 0 0 0 0 0 0)}
             (:digital-out @board)))
      (is (= [{:data [], :name "PIN0", :num 0, :wave "x.."}
              {:data [], :name "PIN1", :num 1, :wave "x.."}
              {:data [], :name "CLK", :num 38, :wave "xhl"}
              {:data [], :name "IO", :num 39, :wave "x.."}
              {:data [], :name "CE/_RST", :num 40, :wave "x.."}]
             (:signal @board))))

    (testing "analog-read"
      (is (= {:ex-msg "Assert failed: Pin 666 has not been mapped."}
             (catch-all (ccore/analog-read board NOT-A-PIN))))
      (is (= {:ex-msg "Assert failed: Pin 1 (PIN1) is not in ANALOG mode."}
             (catch-all (ccore/analog-read board PIN1))))
      (is (= {:ex-msg "Assert failed: Pin 0 (PIN0) is not in ANALOG mode."}
             (catch-all (ccore/analog-read board PIN0))))
      (is (= {:ex-msg "Assert failed: Pin 38 (CLK) is not in ANALOG mode."}
             (catch-all (ccore/analog-read board CLK))))
      (is (= {:ex-msg "Assert failed: Pin 40 (CE/_RST) is not in ANALOG mode."}
             (catch-all (ccore/analog-read board CE))))
      (is (= "| [43m[30m39[m[m | IO | analog read |  |\n"
             (ccore/analog-read board IO)))
      (is (= nil
             (:analog @board))))

    (testing "analog-write"
      (is (= {:ex-msg "Assert failed: Pin 666 has not been mapped."}
             (catch-all (ccore/analog-write board NOT-A-PIN 0))))
      (is (= {:ex-msg "Assert failed: Pin 39 (IO) is not in PWM mode."}
             (catch-all (ccore/analog-write board IO 0))))
      (is (= {:ex-msg "Assert failed: Pin 0 (PIN0) is not in PWM mode."}
             (catch-all (ccore/analog-write board PIN0 0))))
      (is (= {:ex-msg "Assert failed: Pin 38 (CLK) is not in PWM mode."}
             (catch-all (ccore/analog-write board CLK 0))))
      (is (= {:ex-msg "Assert failed: Pin 40 (CE/_RST) is not in PWM mode."}
             (catch-all (ccore/analog-write board CE 0))))
      (is (= {:ex-msg "Assert failed: PWM value should be between 0 and 255: -1"}
             (catch-all (ccore/analog-write board PIN1 -1))))
      (is (= {:ex-msg "Assert failed: PWM value should be between 0 and 255: 256"}
             (catch-all (ccore/analog-write board PIN1 256))))
      (is (= "| [41m[37m01[m[m | PIN1 | analog write | 42 |\n"
             (ccore/analog-write board PIN1 42)))
      (is (= [{:data [], :name "PIN0", :num 0, :wave "x..."}
              {:data ["42"], :name "PIN1", :num 1, :wave "x..2"}
              {:data [], :name "CLK", :num 38, :wave "xhl."}
              {:data [], :name "IO", :num 39, :wave "x..."}
              {:data [], :name "CE/_RST", :num 40, :wave "x..."}]
             (:signal @board))))

    (testing "analog-write-color"
      (is (= "| [41m[37m01[m[m | PIN1 | analog write | 17 |\n"
             (ccore/analog-write board PIN1 17)))
      (is (= {:data ["42" "17"], :name "PIN1", :num 1, :wave "x..23"}
             (-> @board :signal second)))
      (is (= "| [41m[37m01[m[m | PIN1 | analog write | 19 |\n"
             (ccore/analog-write board PIN1 19)))
      (is (= {:data ["42" "17" "19"], :name "PIN1", :num 1, :wave "x..234"}
             (-> @board :signal second)))
      (is (= "| [41m[37m01[m[m | PIN1 | analog write | 21 |\n"
             (ccore/analog-write board PIN1 21)))
      (is (= {:data ["42" "17" "19" "21"], :name "PIN1", :num 1, :wave "x..2345"}
             (-> @board :signal second)))
      (is (= "| [41m[37m01[m[m | PIN1 | analog write | 23 |\n"
             (ccore/analog-write board PIN1 23)))
      (is (= {:data ["42" "17" "19" "21" "23"], :name "PIN1", :num 1, :wave "x..23456"}
             (-> @board :signal second)))
      (is (= "| [41m[37m01[m[m | PIN1 | analog write | 24 |\n"
             (ccore/analog-write board PIN1 24)))
      (is (= {:data ["42" "17" "19" "21" "23" "24"], :name "PIN1", :num 1, :wave "x..234567"}
             (-> @board :signal second)))
      (is (= "| [41m[37m01[m[m | PIN1 | analog write | 27 |\n"
             (ccore/analog-write board PIN1 27)))
      (is (= {:data ["42" "17" "19" "21" "23" "24" "27"], :name "PIN1", :num 1, :wave "x..2345678"}
             (-> @board :signal second)))
      (is (= "| [41m[37m01[m[m | PIN1 | analog write | 30 |\n"
             (ccore/analog-write board PIN1 30)))
      (is (= {:data ["42" "17" "19" "21" "23" "24" "27" "30"], :name "PIN1", :num 1, :wave "x..23456789"}
             (-> @board :signal second)))
      (is (= "| [41m[37m01[m[m | PIN1 | analog write | 32 |\n"
             (ccore/analog-write board PIN1 32)))
      (is (= {:data ["42" "17" "19" "21" "23" "24" "27" "30" "32"], :name "PIN1", :num 1, :wave "x..234567892"}
             (-> @board :signal second))))

    (testing "enable-pin"
      (is (= {:ex-msg "Unknown pin type." :ex-data {}}
             (catch-all (ccore/enable-pin board :unknown PIN0))))
      (is (= {:ex-msg "Assert failed: Pin 666 has not been mapped."}
             (catch-all (ccore/enable-pin board :analog NOT-A-PIN))))
      (is (= {:ex-msg "Assert failed: Pin 666 has not been mapped."}
             (catch-all (ccore/enable-pin board :digital NOT-A-PIN))))
      (testing "analog"
        (is (= {:ex-msg "Assert failed: Pin 0 (PIN0) should be in ANALOG, PWM or SERVO mode if enabled as :analog."}
               (catch-all (ccore/enable-pin board :analog PIN0))))
        (is (= "| [41m[37m01[m[m | PIN1 | enable | with code 193 1 |\n"
               (catch-all (ccore/enable-pin board :analog PIN1))))
        (is (= {:ex-msg "Assert failed: Pin 38 (CLK) should be in ANALOG, PWM or SERVO mode if enabled as :analog."}
               (catch-all (ccore/enable-pin board :analog CLK))))
        (is (= "| [43m[30m39[m[m | IO | enable | with code 231 1 |\n"
               (ccore/enable-pin board :analog IO)))
        (is (= "| [45m[37m40[m[m | CE/_RST | enable | with code 232 1 |\n"
               (ccore/enable-pin board :analog CE))))
      (testing "digital"
        (is (= "| [40m[37m00[m[m | PIN0 | enable | with code 208 1 |\n"
               (ccore/enable-pin board :digital PIN0)))
        (is (= {:ex-msg "Assert failed: Pin 1 (PIN1) should be in ANALOG or PWM mode if enabled as :analog."}
               (catch-all (ccore/enable-pin board :digital PIN1))))
        (is (= "| [47m[30m38[m[m | CLK | enable | with code 212 1 |\n"
               (ccore/enable-pin board :digital CLK)))
        (is (= {:ex-msg "Assert failed: Pin 39 (IO) should be in ANALOG or PWM mode if enabled as :analog."}
               (catch-all (ccore/enable-pin board :digital IO))))
        (is (= {:ex-msg "Assert failed: Pin 40 (CE/_RST) should be in ANALOG or PWM mode if enabled as :analog."}
               (catch-all (ccore/enable-pin board :digital CE))))))

    (testing "disable-pin"
      (is (= {:ex-msg "Unknown pin type." :ex-data {}}
             (catch-all (ccore/disable-pin board :unknown PIN0))))
      (is (= {:ex-msg "Assert failed: Pin 666 has not been mapped."}
             (catch-all (ccore/disable-pin board :analog NOT-A-PIN))))
      (is (= {:ex-msg "Assert failed: Pin 666 has not been mapped."}
             (catch-all (ccore/disable-pin board :digital NOT-A-PIN))))
      (testing "analog"
        (is (= {:ex-msg "Assert failed: Pin 0 (PIN0) should be in ANALOG, PWM or SERVO mode if disabled as :analog."}
               (catch-all (ccore/disable-pin board :analog PIN0))))
        (is (= "| [41m[37m01[m[m | PIN1 | disable | with code 193 0 |\n"
               (catch-all (ccore/disable-pin board :analog PIN1))))
        (is (= {:ex-msg "Assert failed: Pin 38 (CLK) should be in ANALOG, PWM or SERVO mode if disabled as :analog."}
               (catch-all (ccore/disable-pin board :analog CLK))))
        (is (= "| [43m[30m39[m[m | IO | disable | with code 231 0 |\n"
               (ccore/disable-pin board :analog IO)))
        (is (= "| [45m[37m40[m[m | CE/_RST | disable | with code 232 0 |\n"
               (ccore/disable-pin board :analog CE))))
      (testing "digital"
        (is (= "| [40m[37m00[m[m | PIN0 | disable | with code 208 0 |\n"
               (ccore/disable-pin board :digital PIN0)))
        (is (= {:ex-msg "Assert failed: Pin 1 (PIN1) should be in ANALOG or PWM mode if disabled as :analog."}
               (catch-all (ccore/disable-pin board :digital PIN1))))
        (is (= "| [47m[30m38[m[m | CLK | disable | with code 212 0 |\n"
               (ccore/disable-pin board :digital CLK)))
        (is (= {:ex-msg "Assert failed: Pin 39 (IO) should be in ANALOG or PWM mode if disabled as :analog."}
               (catch-all (ccore/disable-pin board :digital IO))))
        (is (= {:ex-msg "Assert failed: Pin 40 (CE/_RST) should be in ANALOG or PWM mode if disabled as :analog."}
               (catch-all (ccore/disable-pin board :digital CE))))))

    (is (= (slurp "test-resources/all-test.md")
           (slurp (str "./output/" output-name ".md"))))

    (is (nil? (ccore/close board)))
    ;;
    ))

(deftest color-test
  (let [board (debug-core/connect :pin-mapping pin-mapping2
                                  :debug true
                                  :output-name "color-test")]
    (doseq [pin-num (conj (range 0 6) 38 39 40)]
      (ccore/pin-mode board pin-num INPUT))
    (is (= (slurp "test-resources/color-test.md")
           (slurp "output/color-test.md")))))

(deftest pin-and-pin-info-test
  (let [board (debug-core/connect :pin-mapping pin-mapping
                                  :debug true
                                  :output-name "pin-and-pin-info-test")]
    (is (= :firmata-debug
           (:interface @board)))

    (ccore/pin-mode board PIN0 INPUT)
    (ccore/pin-mode board PIN1 PWM)
    (ccore/pin-mode board CLK OUTPUT)
    (ccore/pin-mode board IO ANALOG)
    (ccore/pin-mode board CE SERVO)

    (testing "pins"
      (is (= {0 {:num 0 :color :black :name "PIN0"}
              1 {:num 1 :color :red :name "PIN1"}
              38 {:num 38 :color :white :name "CLK"}
              39 {:num 39 :color :yellow :name "IO"}
              40 {:num 40 :color :magenta :name "CE/_RST"}}
             (sut/pins pin-mapping))))

    (testing "pin-info"
      (is (= {:num 0 :color :black :name "PIN0" :port 0 :mode INPUT
              :pin-modes-on-port [INPUT PWM nil nil nil nil nil nil]}
             (sut/pin-info board PIN0)))
      (is (= {:num 1 :color :red :name "PIN1" :port 0 :mode PWM
              :pin-modes-on-port [INPUT PWM nil nil nil nil nil nil]}
             (sut/pin-info board PIN1)))
      (is (= {:num 38 :color :white :name "CLK" :port 4 :mode OUTPUT
              :pin-modes-on-port [nil nil nil nil nil nil OUTPUT ANALOG]}
             (sut/pin-info board CLK)))
      (is (= {:num 39 :color :yellow :name "IO" :port 4 :mode ANALOG
              :pin-modes-on-port [nil nil nil nil nil nil OUTPUT ANALOG]}
             (sut/pin-info board IO)))
      (is (= {:num 40 :color :magenta :name "CE/_RST" :port 5 :mode SERVO
              :pin-modes-on-port [SERVO nil nil nil nil nil nil nil]}
             (sut/pin-info board CE))))))

(deftest digital-write-test
  (let [board (debug-core/connect :pin-mapping pin-mapping
                                  :debug true
                                  :output-name "digital-write-test")]
    (is (= :firmata-debug
           (:interface @board)))

    (ccore/pin-mode board CLK OUTPUT)
    (ccore/pin-mode board PIN1 OUTPUT)

    (is (= "| [47m[30m38[m[m | CLK | digital-write | [42m HIGH [m |\n"
           (ccore/digital-write board CLK HIGH)))
    (is (= "| [41m[37m01[m[m | PIN1 | digital-write | [40m LOW [m |\n"
           (ccore/digital-write board PIN1 sut/LOW_ARROW)))
    (is (= [{:data [], :name "PIN0", :num 0, :wave "x.."}
            {:data [], :name "PIN1", :num 1, :wave "x.L"}
            {:data [], :name "CLK", :num 38, :wave "xh."}
            {:data [], :name "IO", :num 39, :wave "x.."}
            {:data [], :name "CE/_RST", :num 40, :wave "x.."}]
           (:signal @board)))
    (is (= {0 '(0 6 0 0 0 0 0 0)
            1 '(0 0 0 0 0 0 0 0)
            2 '(0 0 0 0 0 0 0 0)
            3 '(0 0 0 0 0 0 0 0)
            4 '(0 0 0 0 0 0 1 0)
            5 '(0 0 0 0 0 0 0 0)
            6 '(0 0 0 0 0 0 0 0)}
           (:digital-out @board))
        "should update the board")
    (is (= {:ex-msg "Assert failed: Pin 38 (CLK) has already value h."}
           (catch-all (ccore/digital-write board CLK HIGH))))
    (is (= "| [41m[37m01[m[m | PIN1 | digital-write | [40m LOW [m |\n"
           (catch-all (ccore/digital-write board PIN1 sut/LOW_ARROW))))))

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
   {:num 1 :color :magenta :name "PIN--1"}])

(deftest two-pins-same-name-test
  (is (= {:ex-msg "Assert failed: The pin PIN0 has been mapped several times. The pin PIN1 has been mapped several times"}
         (catch-all (debug-core/connect :pin-mapping pin-mapping3
                                        :debug true
                                        :output-filename "nok-test.md")))))

(deftest two-pins-same-num-test
  (is (= {:ex-msg "Assert failed: Pin number 00 has been mapped several times. Pin number 01 has been mapped several times"}
         (catch-all (debug-core/connect :pin-mapping pin-mapping4
                                        :debug true
                                        :output-filename "nok-test.md")))))

(deftest incorrect-pin-mapping-test
  (is (= {:ex-msg "Assert failed: One of the pins' color is not an allowed keyword"}
         (catch-all (debug-core/connect :pin-mapping pin-mapping5
                                        :debug true
                                        :output-filename "nok-test.md")))))

(sut/display-all-ansi-colors)