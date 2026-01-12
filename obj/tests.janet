(import ./errors :prefix "")
(import ./rewrite :prefix "")
(import ./paths :prefix "")

(def t/test-file-ext ".niche")

(defn t/make-test-path
  [in-path]
  (def [fdir fname] (p/parse-path in-path))
  #
  (string fdir "_" fname t/test-file-ext))

(comment

  (t/make-test-path "tmp/hello.janet")
  # =>
  "tmp/_hello.janet.niche"

  )

(defn t/make-tests
  [in-path &opt opts]
  (def b {:in "make-tests" :args {:in-path in-path :opts opts}})
  #
  (def src (slurp in-path))
  (def [ok? _] (protect (parse-all src)))
  (when (not ok?)
    (break :parse-error))
  #
  (def test-src (r/rewrite-as-test-file src))
  (when (not test-src)
    (break nil))
  #
  (def test-path (t/make-test-path in-path))
  (when (and (not (get opts :overwrite))
             (os/stat test-path :mode))
    (e/emf (merge b {:locals {:test-path test-path}})
           "test file already exists for: %s" in-path))
  #
  (spit test-path test-src)
  #
  test-path)

(defn t/run-tests
  [test-path]
  (def b {:in "run-tests" :args {:test-path test-path}})
  #
  (try
    (with [of (file/temp)]
      (with [ef (file/temp)]
        (let [# prevents any contained `main` functions from executing
              cmd
              ["janet" "-e" (string "(dofile `" test-path "`)")]
              ecode
              (os/execute cmd :p {:out of :err ef})]
          #
          (file/flush of)
          (file/flush ef)
          (file/seek of :set 0)
          (file/seek ef :set 0)
          # XXX: iiuc ecode cannot be nil
          [ecode
           (file/read of :all)
           (file/read ef :all)])))
    ([e]
      (e/emf (merge b {:e-via-try e})
             "problem running tests in: %s" test-path))))

(defn t/parse-output
  [out]
  (def b {:in "parse-output" :args {:out out}})
  # see verify.janet
  (def boundary (buffer/new-filled 72 (chr "#")))
  (def b-idx (last (string/find-all boundary out)))
  (when (not b-idx)
    (e/emf b "failed to find boundary in output: %n" out))
  #
  (def [test-out results] (string/split boundary out b-idx))
  #
  [(parse results) test-out])

(comment

  (def data
    {:test-form '(+ 1 1)
     :test-status true
     :test-value 2
     :expected-form 3
     :expected-status true
     :expected-value 3
     :line-no 4
     :passed true
     :name ""})

  (def separator (buffer/new-filled 72 (chr "#")))

  (def out
    (string
      "hello this is a line\n"
      "and so is this\n"
      separator "\n"
      (string/format "%j" data)))

  (t/parse-output out)
  # =>
  [{:expected-form 3
    :expected-status true
    :expected-value 3
    :line-no 4
    :name ""
    :passed true
    :test-form '(+ 1 1)
    :test-status true
    :test-value 2}
   "hello this is a line\nand so is this\n"]

  )

(defn t/make-lint-path
  [in-path]
  #
  (string (t/make-test-path in-path) "-lint"))

(comment

  (t/make-lint-path "tmp/hello.janet")
  # =>
  "tmp/_hello.janet.niche-lint"

  )

(defn t/lint-and-get-error
  [input]
  (def lint-path (t/make-lint-path input))
  (defer (os/rm lint-path)
    (def lint-src (r/rewrite-as-file-to-lint (slurp input)))
    (spit lint-path lint-src)
    (def lint-buf @"")
    (with-dyns [:err lint-buf] (flycheck lint-path))
    # XXX: peg may need work
    (peg/match ~(sequence "error: " (to ":") (capture (to "\n")))
               lint-buf)))

(defn t/has-unreadable?
  [test-results]
  (var unreadable? nil)
  (each f (get test-results :fails)
    (when (get f :test-unreadable)
      (set unreadable? f)
      (break))
    #
    (when (get f :expected-unreadable)
      (set unreadable? f)
      (break)))
  #
  unreadable?)

(defn t/make-and-run
  [input &opt opts]
  (def b @{:in "make-and-run" :args {:input input :opts opts}})
  #
  (default opts @{})
  # create test source
  (def result (t/make-tests input opts))
  (cond
    (not result)
    (break [:no-tests nil nil nil])
    #
    (= :parse-error result)
    (break [:parse-error nil nil nil]))
  #
  (def test-path result)
  # run tests and collect output
  (def [exit-code out err] (t/run-tests test-path))
  (os/rm test-path)
  #
  (when (empty? out)
    (if (t/lint-and-get-error input)
      (break [:lint-error nil nil nil])
      (break [:test-run-error nil nil nil])))
  #
  (def [test-results test-out] (t/parse-output out))
  (when-let [unreadable (t/has-unreadable? test-results)]
    (e/emf b (string/format "unreadable value in:\n%s"
                            (if (dyn :test/color?) "%M" "%m"))
           unreadable))
  #
  [exit-code test-results test-out err])

