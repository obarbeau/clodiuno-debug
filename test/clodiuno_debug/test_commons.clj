(ns clodiuno-debug.test-commons)

(def output-name "all-test")
(def PIN0 0)
(def PIN1 1)
(def CLK 38)
(def IO  39)
(def CE  40)
(def NOT-A-PIN 666)
(def pin-mapping [{:num PIN0 :color :black  :name "PIN0"}
                  {:num PIN1 :color :red    :name "PIN1"}
                  {:num CLK  :color :white  :name "CLK"}
                  {:num IO   :color :yellow :name "IO"}
                  {:num CE   :color :magenta :name "CE/_RST"}])
