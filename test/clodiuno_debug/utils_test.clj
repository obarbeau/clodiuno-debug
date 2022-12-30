(ns clodiuno-debug.utils-test
  (:require [clodiuno-debug.utils :as sut]
            [clodiuno-debug.test-commons :refer [pin-mapping]]
            [clodiuno-debug.test-helpers :refer [catch-all with-logged-event-maps]]
            [clojure.test :refer [deftest is]]))

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
