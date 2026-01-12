# XXX: try to put in file?  had trouble originally when working on
#      judge-gen.  may be will have more luck?
(def v/as-string
  ``
  # influenced by janet's tools/helper.janet

  (var _verify/start-time 0)
  (var _verify/end-time 0)
  (var _verify/test-results @[])

  (defmacro _verify/is
    [t-form e-form line-no name]
    (with-syms [$ts $tr
                $es $er]
      ~(do
         (def [,$ts ,$tr] (protect (eval ',t-form)))
         (def [,$es ,$er] (protect (eval ',e-form)))
         (array/push _verify/test-results
                     @{:test-form ',t-form
                       :test-status ,$ts
                       :test-value ,$tr
                       #
                       :expected-form ',e-form
                       :expected-status ,$es
                       :expected-value ,$er
                       #
                       :line-no ,line-no
                       :name ,name
                       :passed (if (and ,$ts ,$es)
                                 (deep= ,$tr ,$er)
                                 nil)})
         ,name)))

  (defn _verify/start-tests
    []
    (set _verify/start-time (os/clock))
    (set _verify/test-results @[]))

  (defn _verify/end-tests
    []
    (set _verify/end-time (os/clock)))

  (defn _verify/report
    []
    # find and massage failures
    (def fails
      (keep (fn [r]
              (when (not (get r :passed))
                (def t-value (get r :test-value))
                (def [tr ts] (protect (string/format "%j" t-value)))
                (when (not tr)
                  (-> r
                      (put :test-value (string/format "%m" t-value))
                      (put :test-unreadable true)))
                (def e-value (get r :expected-value))
                (def [er es] (protect (string/format "%j" e-value)))
                (when (not er)
                  (-> r
                      (put :expected-value (string/format "%m" e-value))
                      (put :expected-unreadable true)))
                #
                r))
            _verify/test-results))
    # prepare test results
    (def test-results
      @{:num-tests (length _verify/test-results)
        :fails fails})
    # output a separator before the test output
    (print (string/repeat "#" 72) "\n")
    # report test results
    (printf "%j" test-results)
    # signal if there were any failures
    (when (not (empty? fails))
      (os/exit 1)))
  ``)

