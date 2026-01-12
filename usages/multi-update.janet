(import ../src/main)
(import ../src/tests :as t)

# preparation for tests
(comment

  (def fnames ["one.janet" "two.janet"])

  (def tmp-dir "tmp")

  (def src-paths (map |(string "data/multi-update/" $) fnames))

  (def dst-paths (map |(string tmp-dir "/" $) fnames))

  (def test-paths (map |(t/make-test-path $) dst-paths))

  # XXX: better to start anew?
  (os/mkdir tmp-dir)

  (each dp dst-paths
    (when (os/stat dp :mode) (os/rm dp)))

  (all |(nil? (os/stat $ :mode)) dst-paths)
  # =>
  true

  (each tp test-paths
    (when (os/stat tp :mode) (os/rm tp)))

  (all |(nil? (os/stat $ :mode)) test-paths)
  # =>
  true

  (def srcs (map slurp src-paths))

  (each [dp s] (map tuple dst-paths srcs)
    (spit dp s))

  (map slurp dst-paths)
  # =>
  srcs

  )

(comment

  (let [buf @""]
    (with-dyns [:out buf]
      (main/main ""
                 (string "{:raw true\n"
                         " :no-color true\n"
                         " :no-exit true}")
                 ;dst-paths))
    (parse buf))
  # =>
  ~@[[,(get dst-paths 0)
       @{:fails @[@{:expected-form @[:b]
                    :expected-status true
                    :expected-value @[:b]
                    :line-no 4
                    :name ""
                    :passed false
                    :test-form (array/concat @[] :a)
                    :test-status true
                    :test-value @[:a]}]
         :num-tests 1}]
     [,(get dst-paths 1)
       @{:fails @[@{:expected-form @[1 2 0]
                    :expected-status true
                    :expected-value @[1 2 0]
                    :line-no 8
                    :name ""
                    :passed false
                    :test-form (map inc [0 1 2])
                    :test-status true
                    :test-value @[1 2 3]}]
         :num-tests 2}]]

  # since an update was attempted, there should be no failures
  (let [buf @""]
    (with-dyns [:out buf]
      (main/main ""
                 (string "{:raw true\n"
                         " :no-color true\n"
                         " :no-exit true}")
                 ;dst-paths))
    (parse buf))
  # =>
  ~@[[,(get dst-paths 0) @{:fails @[] :num-tests 1}]
     [,(get dst-paths 1) @{:fails @[] :num-tests 2}]]

  )

(comment

  (each dp dst-paths
    (os/rm dp))

  (all |(nil? (os/stat $ :mode)) dst-paths)
  # =>
  true

  (os/rmdir tmp-dir)

  (os/stat tmp-dir :mode)
  # =>
  nil

  )

