(def p/sep
  (let [tos (os/which)]
    (if (or (= :windows tos) (= :mingw tos)) `\` "/")))

(defn p/clean-end-of-path
  [path a-sep]
  (when (one? (length path))
    (break path))
  (if (string/has-suffix? a-sep path)
    (string/slice path 0 -2)
    path))

(comment

  (p/clean-end-of-path "hello/" "/")
  # =>
  "hello"

  (p/clean-end-of-path "/" "/")
  # =>
  "/"

  )

(defn p/parse-path
  [path]
  (def revcap-peg
    ~(sequence (capture (sequence (choice (to (choice "/" `\`))
                                          (thru -1))))
               (capture (thru -1))))
  (when-let [[rev-name rev-dir]
             (-?>> (string/reverse path)
                   (peg/match revcap-peg)
                   (map string/reverse))]
    [(or rev-dir "") rev-name]))

(comment

  (p/parse-path "/tmp/fun/my.fnl")
  # =>
  ["/tmp/fun/" "my.fnl"]

  (p/parse-path "/my.janet")
  # =>
  ["/" "my.janet"]

  (p/parse-path "pp.el")
  # =>
  ["" "pp.el"]

  (p/parse-path "/etc/init.d")
  # =>
  ["/etc/" "init.d"]

  (p/parse-path "/")
  # =>
  ["/" ""]

  (p/parse-path "")
  # =>
  ["" ""]

  )

