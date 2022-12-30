(ns clodiuno-debug.test-helpers
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

(defmacro catch-all
  "Helper useful to use in some tests.
  Typically if you expect some exception to be thrown, or simply
  if you want a narrower error message if an exception is thrown in a test.

  Example:
  ```
  (is (= expected-value (catch-all (my-function ,,,))))
  ```

  will show a failure at the `is` level and not at the global `deftest`.
  "
  {:style/indent 0}
  [& body]
  `(try
     (do ~@body)
     (catch Exception e#
       (if-let [data# (ex-data e#)]
         {:ex-msg (ex-message e#)
          :ex-data data#}
         {:ex-msg (ex-message e#)
          :ex-cause (ex-cause e#)
          :e e#}))
     (catch AssertionError ae#
       {:ex-msg (-> (ex-message ae#) str/split-lines first)})))

(defmacro with-logged-event-maps
  {:style/indent 1}
  [event-maps-name & body]
  {:pre [(simple-symbol? event-maps-name)]}
  ;; Avoid data races by using `gensym`:
  (let [key-name (-> event-maps-name str gensym keyword)]
    `(let [dest# (atom [])
           ~event-maps-name dest#
           previous-level# (:level log/*config*)
           previous-ns-whitelist# (:ns-whitelist log/*config*)
           previous-ns-blacklist# (:ns-blacklist log/*config*)
           previous-appenders# (:appenders log/*config*)
           appender# {:enabled? true
                      :async? false
                      :min-level :debug
                      :fn (fn [data#]
                            (swap! dest# conj
                                   {:msg (force (:msg_ data#))
                                    :level (:level data#)}))}]
       (try
         (log/swap-config! (fn [old#]
                             (-> old#
                                 (assoc :level :debug
                                        :ns-blacklist []
                                        :appenders {~key-name appender#})
                                 (dissoc :ns-whitelist))))
         (do ~@body)
         (finally
           (log/swap-config! (fn [old#]
                               (-> old#
                                   (assoc :level previous-level#
                                          :ns-blacklist previous-ns-blacklist#
                                          :ns-whitelist previous-ns-whitelist#
                                          :appenders previous-appenders#)))))))))

(defn delete-output-dir []
  (doseq [ff (.listFiles (io/file "./output"))]
    (io/delete-file ff)))