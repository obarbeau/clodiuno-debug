(ns clodiuno-debug.timbre-config
  "╭──────────────────────╮
   │ Timbre configuration │
   ╰──────────────────────╯"
  (:require [clojure.string :as str]
            [io.aviso.ansi :as ansi]
            [taoensso.encore :as enc]
            [taoensso.timbre :as log]
            [taoensso.timbre.appenders.core :as appenders]))

(set! *warn-on-reflection* true)

;; log configuration
;; -----------------

(def log-file-name "./output/clodiuno-debug.log")

(defn color-output
  "Quite the same as timbre's `default-output-fn` but with color, and no hostname."
  ([data] (color-output nil data))
  ([opts data]
   (let [{:keys [no-stacktrace?]} opts
         {:keys [level ?err msg_ ?ns-str ?file timestamp_ ?line]} data
         ansi-color (condp = level
                      :trace ansi/blue-font :debug ansi/blue-font
                      :info ansi/green-font :warn ansi/yellow-font
                      :error ansi/red-font :fatal ansi/red-font
                      :report ansi/black-font ansi/black-font)]
     (str
      ansi/black-font (when-let [ts (force timestamp_)] (str ts " "))
      ansi/reset-font
      ansi-color (format "%1$5s" (str/upper-case (name level)))
      ansi/reset-font " "
      ansi/blue-font "["
      (format "%1$25s" (apply str (take-last 25 (or ?ns-str ?file "?")))) ":" (or ?line "?") "] - "
      ansi/reset-font
      ansi-color (force msg_) ansi/reset-font
      (when-not no-stacktrace?
        (when-let [err ?err]
          (str enc/system-newline (log/stacktrace err opts))))))))

(defn only-level-and-message
  "Nothing else than the level and message for this appender."
  ([appender-data]
   (let [{:keys [level ?err msg_]} appender-data]
     (str (str/upper-case (name level))  " "
          (force msg_)
          (when-let [err ?err]
            (str enc/system-newline (log/stacktrace err)))))))

(log/merge-config!
 {:min-level :debug
  :ns-filter #{"*"}
  :middleware []
  :timestamp-opts {:pattern "yyMMdd HH:mm"}
  :appenders {:spit (merge (appenders/spit-appender {:fname log-file-name})
                           {:output-fn color-output})
              :println {:enabled? true
                        :output-fn only-level-and-message}}})

(comment
  (log/info log/*config*))