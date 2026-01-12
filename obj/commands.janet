(import ./errors :prefix "")
(import ./files :prefix "")
(import ./log :prefix "")
(import ./output :prefix "")
(import ./rewrite :prefix "")
(import ./tests :prefix "")

########################################################################

(defn c/summarize
  [noted-paths]
  # pass / fail
  (def ps-paths (get noted-paths :pass))
  (def fl-paths (get noted-paths :fail))
  #
  (when fl-paths
    (def n-ps-paths (length ps-paths))
    (def n-fl-paths (length fl-paths))
    (when (empty? fl-paths)
      (l/notenf :i "All tests successful in %d file(s)."
                n-ps-paths))
    (when (not (empty? fl-paths))
      (l/notenf :i "Test failures in %d of %d file(s)."
                n-fl-paths (+ n-fl-paths n-ps-paths))))
  # updated
  (def upd-paths (get noted-paths :update))
  #
  (when upd-paths
    (def n-upd-paths (length upd-paths))
    (l/notenf :i "Test(s) updated in %d file(s)." n-upd-paths))
  # errors
  (def p-paths (get noted-paths :parse))
  (def l-paths (get noted-paths :lint))
  (def r-paths (get noted-paths :run))
  (def err-paths [p-paths l-paths r-paths])
  #
  (when (some |(not (empty? $)) err-paths)
    (def num-skipped (sum (map length err-paths)))
    (l/notenf :w "Skipped %d files(s)." num-skipped))
  (when (not (empty? p-paths))
    (l/notenf :w "%s: parse error(s) detected in %d file(s)."
              (o/color-msg "WARNING" :red) (length p-paths)))
  (when (not (empty? l-paths))
    (l/notenf :w "%s: linting error(s) detected in %d file(s)."
              (o/color-msg "WARNING" :yellow) (length l-paths)))
  (when (not (empty? r-paths))
    (l/notenf :w "%s: runtime error(s) detected for %d file(s)."
              (o/color-msg "WARNING" :yellow) (length r-paths))))

(defn c/mru-single
  [input &opt opts]
  (def b @{:in "mru-single" :args {:input input :opts opts}})
  # try to make and run tests, then collect output
  (def [exit-code test-results _ _] (t/make-and-run input opts))
  (when (get (invert [:no-tests
                      :parse-error :lint-error :test-run-error])
             exit-code)
    (break [exit-code nil nil]))
  # successful run means no tests to update
  (when (zero? exit-code)
    (break [:no-updates nil test-results]))
  #
  (def fails (get test-results :fails))
  (def update-info
    (seq [f :in (if (get opts :update-first)
                  @[(get fails 0)]
                  fails)
          :let [{:line-no line-no :test-value test-value} f
                tv-str (string/format "%j" test-value)]]
      [line-no tv-str]))
  (def ret (r/patch input update-info))
  (when (not ret)
    (e/emf (merge b {:locals {:fails fails :update-info update-info}})
           "failed to patch: %n" input))
  #
  (def lines (map |(get $ 0) update-info))
  #
  (if (get opts :update-first)
    [:single-update lines test-results]
    [:multi-update lines test-results]))

(defn c/tally-mru-result
  [path [desc data tr] noted-paths]
  (def b @{:in "tally-mru-result"
           :args {:path path :single-result [desc data tr]
                  :noted-paths noted-paths}})
  #
  (var ret nil)
  (case desc
    :no-tests
    (l/noten :i " - no tests found")
    #
    :parse-error
    (let [msg (o/color-msg "detected parse errors" :red)]
      (l/notenf :w " - %s" msg)
      (array/push (get noted-paths :parse) path))
    #
    :lint-error
    (let [msg (o/color-msg "detected lint errors" :yellow)]
      (l/notenf :w " - %s" msg)
      (array/push (get noted-paths :lint) path))
    #
    :test-run-error
    (let [msg (o/color-msg "test file had runtime errors" :yellow)]
      (l/notenf :w " - %s" msg)
      (array/push (get noted-paths :run) path))
    #
    :no-updates
    (l/noten :i " - no tests needed updating")
    #
    :multi-update
    (let [cs-lines (string/join (map |(string $) data) ", ")
          raw-msg (string/format "test(s) updated on line(s): %s"
                                 cs-lines)
          msg (o/color-msg raw-msg :green)]
      (array/push (get noted-paths :update) path)
      (l/notenf :i " - %s" msg))
    #
    :single-update
    (let [first-line (get data 0)
          raw-msg (string/format "test updated on line: %d"
                                 first-line)
          msg (o/color-msg raw-msg :green)]
      (array/push (get noted-paths :update) path)
      (l/notenf :i " - %s" msg)
      (set ret :halt))
    #
    (e/emf b "unexpected result %n for: %s" desc path))
  #
  ret)

(defn c/make-run-update
  [src-paths opts]
  (def excludes (get opts :excludes))
  (def noted-paths @{:parse @[] :lint @[] :run @[]
                     :update @[]})
  (def test-results @[])
  # generate tests, run tests, and update
  (each path src-paths
    (when (and (not (has-value? excludes path)) (f/is-file? path))
      (l/note :i path)
      (def single-result (c/mru-single path opts))
      (def [_ _ tr] single-result)
      (array/push test-results [path tr])
      (def ret (c/tally-mru-result path single-result noted-paths))
      (when (= :halt ret)
        (break))))
  #
  (l/notenf :i (o/separator "="))
  (c/summarize noted-paths)
  #
  (def exit-code 0)
  #
  [exit-code test-results])

