(import ./errors :as e)
(import ./files :as f)
(import ./log :as l)
(import ./output :as o)
(import ./rewrite :as r)
(import ./tests :as t)

########################################################################

(defn summarize
  [noted-paths]
  # updated
  (def upd-paths (get noted-paths :update))
  #
  (when upd-paths
    (def n-upd-paths (length upd-paths))
    (l/notenf :i "Test(s) updated in %d file(s)." n-upd-paths)))

(defn mru-single
  [input &opt opts]
  (def b @{:in "mru-single" :args {:input input :opts opts}})
  # try to make and run tests, then collect output
  (def [exit-code test-results _ _] (t/make-and-run input opts))
  (when (= :no-tests exit-code)
    (break [:no-tests nil nil]))
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

(defn tally-mru-result
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

(defn make-run-update
  [src-paths opts]
  (def excludes (get opts :excludes))
  (def noted-paths @{:update @[]})
  (def test-results @[])
  # generate tests, run tests, and update
  (each path src-paths
    (when (and (not (has-value? excludes path)) (f/is-file? path))
      (l/note :i path)
      (def single-result (mru-single path opts))
      (def [_ _ tr] single-result)
      (array/push test-results [path tr])
      (def ret (tally-mru-result path single-result noted-paths))
      (when (= :halt ret)
        (break))))
  #
  (l/notenf :i (o/separator "="))
  (summarize noted-paths)
  #
  (def exit-code 0)
  #
  [exit-code test-results])

