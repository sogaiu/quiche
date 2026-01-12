(import ./errors :as e)
(import ./files :as f)
(import ./settings :as s)

(defn parse-args
  [args]
  (def b {:in "parse-args" :args {:args args}})
  #
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (when (or (= head "-h") (= head "--help")
            # might have been invoked with no paths in repository root
            (and (not head) (not (f/is-file? s/conf-file))))
    (break @{:show-help true}))
  #
  (when (or (= head "-v") (= head "--version")
            # might have been invoked with no paths in repository root
            (and (not head) (not (f/is-file? s/conf-file))))
    (break @{:show-version true}))
  #
  (def opts
    (if head
      (if-not (and (string/has-prefix? "{" head)
                   (string/has-suffix? "}" head))
        @{}
        (let [parsed
              (try (parse (string "@" head))
                ([e] (e/emf (merge b {:e-via-try e})
                            "failed to parse options: %n" head)))]
          (when (not (and parsed (table? parsed)))
            (e/emf b "expected table but found: %s" (type parsed)))
          #
          (array/remove the-args 0)
          parsed))
      @{}))
  #
  (def [includes excludes]
    (cond
      # paths on command line take precedence over conf file
      (not (empty? the-args))
      [the-args @[]]
      # conf file
      (f/is-file? s/conf-file)
      (s/parse-conf-file s/conf-file)
      #
      (e/emf b "unexpected result parsing args: %n" args)))
  #
  (setdyn :test/color?
          (not (or (os/getenv "NO_COLOR") (get opts :no-color))))
  #
  (defn merge-indexed
    [left right]
    (default left [])
    (default right [])
    (distinct [;left ;right]))
  #
  (merge opts
         {:includes (merge-indexed includes (get opts :includes))
          :excludes (merge-indexed excludes (get opts :excludes))}))

(comment

  (def old-value (dyn :test/color?))

  (setdyn :test/color? false)

  (parse-args ["src/main.janet"])
  # =>
  @{:excludes @[]
    :includes @["src/main.janet"]}

  (parse-args ["-h"])
  # =>
  @{:show-help true}

  (parse-args ["{:overwrite true}" "src/main.janet"])
  # =>
  @{:excludes @[]
    :includes @["src/main.janet"]
    :overwrite true}

  (parse-args [`{:excludes ["src/args.janet"]}` "src/main.janet"])
  # =>
  @{:excludes @["src/args.janet"]
    :includes @["src/main.janet"]}

  (setdyn :test/color? old-value)

  )

