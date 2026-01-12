(def sep
  (let [tos (os/which)]
    (if (or (= :windows tos) (= :mingw tos)) `\` "/")))

(defn clean-end-of-path
  [path a-sep]
  (when (one? (length path))
    (break path))
  (if (string/has-suffix? a-sep path)
    (string/slice path 0 -2)
    path))

(comment

  (clean-end-of-path "hello/" "/")
  # =>
  "hello"

  (clean-end-of-path "/" "/")
  # =>
  "/"

  )

(defn parse-path
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

  (parse-path "/tmp/fun/my.fnl")
  # =>
  ["/tmp/fun/" "my.fnl"]

  (parse-path "/my.janet")
  # =>
  ["/" "my.janet"]

  (parse-path "pp.el")
  # =>
  ["" "pp.el"]

  (parse-path "/etc/init.d")
  # =>
  ["/etc/" "init.d"]

  (parse-path "/")
  # =>
  ["/" ""]

  (parse-path "")
  # =>
  ["" ""]

  )

