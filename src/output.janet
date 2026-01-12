(import ./log :as l)

(def color-table
  {:black 30
   :blue 34
   :cyan 36
   :green 32
   :magenta 35
   :red 31
   :white 37
   :yellow 33})

(defn color-msg
  [msg color]
  (def color-num (get color-table color))
  (assertf color-num "unknown color: %n" color)
  #
  (if (dyn :test/color?)
    (string "\e[" color-num "m" msg "\e[0m")
    msg))

(defn prin-color
  [msg color]
  (l/note :o (color-msg msg color)))

(comment

  (def [ok? result] (protect (prin-color "hey" :chartreuse)))
  # =>
  [false "unknown color: :chartreuse"]

  )

(defn separator
  [&opt str n]
  (default str "-")
  (default n 60)
  (string/repeat str n))

(defn prin-sep
  [&opt str n]
  (default str "-")
  (default n 60)
  (l/note :o (separator str n)))

(defn prin-form
  [form &opt color]
  (def buf @"")
  (with-dyns [:out buf]
    (printf "%m" form))
  (def msg (string/trimr buf))
  (def m-buf
    (buffer ":\n"
            (if color (color-msg msg color) msg)))
  (l/note :o m-buf))

(defn color-form
  [form]
  (def leader
    (if (or (array? form) (table? form) (buffer? form))
      "@" ""))
  (def fmt-str
    (if (dyn :test/color?) "%M" "%m"))
  (def buf @"")
  (cond
    (indexed? form)
    (do
      (buffer/push buf leader "[\n")
      (each f form
        (with-dyns [:out buf] (printf fmt-str f)))
      (buffer/push buf "]"))
    #
    (dictionary? form)
    (do
      (buffer/push buf leader "{\n")
      (eachp [k v] form
        (with-dyns [:out buf]
          (printf fmt-str k)
          (printf fmt-str v)))
      (buffer/push buf "}"))
    #
    (with-dyns [:out buf] (printf fmt-str form)))
  #
  buf)

(defn color-ratio
  [num denom]
  (buffer (if (not= num denom)
            (color-msg num :red)
            (color-msg num :green))
          "/"
          (color-msg denom :green)))

(defn report-fails
  [{:num-tests total-tests :fails fails}]
  (var i 0)
  (each f fails
    (def {:test-value test-value
          :expected-value expected-value
          :line-no line-no
          :test-form test-form} f)
    (++ i)
    #
    (l/noten :o)
    (l/note :o "[")
    (prin-color i :cyan)
    (l/note :o "]")
    (l/noten :o)
    #
    (l/noten :o)
    (prin-color "failed:" :yellow)
    (l/noten :o)
    (prin-color (string/format "line %d" line-no) :red)
    (l/noten :o)
    #
    (l/noten :o)
    (prin-color "form" :yellow)
    (prin-form test-form)
    (l/noten :o)
    #
    (l/noten :o)
    (prin-color "expected" :yellow)
    (prin-form expected-value)
    (l/noten :o)
    #
    (l/noten :o)
    (prin-color "actual" :yellow)
    (prin-form test-value :blue)
    (l/noten :o)))

(defn report-std
  [content title]
  (when (and content (pos? (length content)))
    (def sepa (separator "-" (length title)))
    (l/noten :o sepa)
    (l/noten :o title)
    (l/noten :o sepa)
    (l/noten :o content)))

(defn report
  [test-results out err]
  (when (not (empty? (get test-results :fails)))
    (l/noten :o)
    (prin-sep)
    #
    (report-fails test-results)
    #
    (when (and out (pos? (length out)))
      (l/noten :o)
      (report-std out "stdout"))
    #
    (when (and err (pos? (length err)))
      (l/noten :o)
      (report-std err "stderr"))
    #
    (when (and (zero? (get test-results :num-tests))
               (empty? out)
               (empty? err))
      (l/noten :o)
      (l/noten :o "no test output...possibly no tests"))
    #
    (prin-sep)
    (l/noten :o)))

