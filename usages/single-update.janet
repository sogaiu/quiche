(import ../src/main)
(import ../src/tests :as t)

# preparation for tests
(comment

  (def fname "one.janet")

  (def tmp-dir "tmp")

  (def src-path (string "data/single-update/" fname))

  (def dst-path (string tmp-dir "/" fname))

  (def test-path (t/make-test-path dst-path))

  # XXX: better to start anew?
  (os/mkdir tmp-dir)

  (when (os/stat dst-path :mode)
    (os/rm dst-path))

  (os/stat dst-path :mode)
  # =>
  nil

  (when (os/stat test-path :mode)
    (os/rm test-path))

  (os/stat test-path :mode)
  # =>
  nil

  (def src (slurp src-path))

  (spit dst-path src)

  (slurp dst-path)
  # =>
  src

  )

(comment

  (let [buf @""]
    (with-dyns [:out buf]
      (main/main ""
                 (string "{:raw true\n"
                         " :no-color true\n"
                         " :no-exit true\n"
                         " :update-first true}")
                 dst-path))
    (parse buf))
  # =>
  ~@[[,dst-path
     @{:fails @[@{:expected-form @[1 2 0]
                  :expected-status true
                  :expected-value @[1 2 0]
                  :line-no 8
                  :name ""
                  :passed false
                  :test-form (map inc [0 1 2])
                  :test-status true
                  :test-value @[1 2 3]}
                @{:expected-form "hello"
                  :expected-status true
                  :expected-value "hello"
                  :line-no 12
                  :name ""
                  :passed false
                  :test-form (print "hello")
                  :test-status true}]
       :num-tests 3}]]

  # since an "update-first" was attempted, there should be one less
  # failure when testing, and the missing one should be the first
  # result.
  (let [buf @""]
    (with-dyns [:out buf]
      (main/main ""
                 (string "{:raw true\n"
                         " :no-color true\n"
                         " :no-exit true}")
                 dst-path))
    (parse buf))
  # =>
  ~@[[,dst-path
      @{:fails @[@{:expected-form "hello"
                   :expected-status true
                   :expected-value "hello"
                   :line-no 12
                   :name ""
                   :passed false
                   :test-form (print "hello")
                   :test-status true}]
        :num-tests 3}]]

  )

(comment

  (when (os/stat dst-path :mode)
    (os/rm dst-path))

  (os/stat dst-path :mode)
  # =>
  nil

  (os/rmdir tmp-dir)

  (os/stat tmp-dir :mode)
  # =>
  nil

  )

