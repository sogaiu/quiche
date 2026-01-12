(import ./errors :as e)
(import ./jipper :as j)
(import ./verify :as v)

# at its simplest, a test is expressed like:
#
# (comment
#
#   (+ 1 1)
#   # =>
#   2
#
#   )
#
# i.e. inside a comment form, a single test consists of:
#
# * a test expression        - `(+ 1 1)`
# * a test indicator         - `# =>`
# * an expected expression   - `2`
#
# there can be one or more tests within a comment form.

# ti == test indicator, which can look like any of:
#
# # =>
# # before =>
# # => after
# # before => after
#
# further constraint that neither `before` nor `after` should contain
# a hash character (#)

(defn find-test-indicator
  [zloc]
  (var label-left nil)
  (var label-right nil)
  [(j/right-until zloc
                  |(match (j/node $)
                     [:comment _ content]
                     (if-let [[l r]
                              (peg/match ~(sequence "#"
                                                    (capture (to "=>"))
                                                    "=>"
                                                    (capture (thru -1)))
                                         content)
                              no-hash-left (nil? (string/find "#" l))
                              no-hash-right (nil? (string/find "#" r))]
                       (do
                         (set label-left (string/trim l))
                         (set label-right (string/trim r))
                         true)
                       false)))
   label-left
   label-right])

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(+ 1 1)" eol
            "# =>"    eol
            "2"))

  (let [[zloc l r]
        (find-test-indicator (-> (j/par src)
                                 j/zip-down))]
    (and zloc
         (empty? l)
         (empty? r)))
  # =>
  true

  (def src
    (string "(+ 1 1)"     eol
            "# before =>" eol
            "2"))

  (let [[zloc l r]
        (find-test-indicator (-> (j/par src)
                                 j/zip-down))]
    (and zloc
         (= "before" l)
         (empty? r)))
  # =>
  true

  (def src
    (string "(+ 1 1)"    eol
            "# => after" eol
            "2"))

  (let [[zloc l r]
        (find-test-indicator (-> (j/par src)
                                 j/zip-down))]
    (and zloc
         (empty? l)
         (= "after" r)))
  # =>
  true

  )

(defn find-test-expr
  [ti-zloc]
  # check for appropriate conditions "before"
  (def before-zlocs @[])
  (var curr-zloc ti-zloc)
  (var found-before nil)
  # collect zlocs to the left of the test indicator up through the
  # first non-whitespace/comment one.  if there is a
  # non-whitespace/comment one, that is the test expression.
  (while curr-zloc
    (set curr-zloc (j/left curr-zloc))
    (when (nil? curr-zloc)
      (break))
    #
    (match (j/node curr-zloc)
      [:comment]
      (array/push before-zlocs curr-zloc)
      #
      [:whitespace]
      (array/push before-zlocs curr-zloc)
      #
      (do
        (set found-before true)
        (array/push before-zlocs curr-zloc)
        (break))))
  #
  (cond
    (nil? curr-zloc)
    :no-test-expression
    # if all collected zlocs (except the last one) are whitespace,
    # then the test expression has been located
    (and found-before
         (->> (slice before-zlocs 0 -2)
              (filter |(not (match (j/node $)
                              [:whitespace]
                              true)))
              length
              zero?))
    curr-zloc
    #
    :unexpected-result))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # =>"           eol
            "  @{:a 2}"        eol
            eol
            "  )"))

  (def [ti-zloc _ _]
    (find-test-indicator (-> (j/par src)
                             j/zip-down
                             j/down)))

  (j/node ti-zloc)
  # =>
  [:comment @{:bc 3 :bl 6 :ec 7 :el 6} "# =>"]

  (def test-expr-zloc (find-test-expr ti-zloc))

  (j/node test-expr-zloc)
  # =>
  [:tuple @{:bc 3 :bl 5 :ec 17 :el 5}
   [:symbol @{:bc 4 :bl 5 :ec 7 :el 5} "put"]
   [:whitespace @{:bc 7 :bl 5 :ec 8 :el 5} " "]
   [:table @{:bc 8 :bl 5 :ec 11 :el 5}]
   [:whitespace @{:bc 11 :bl 5 :ec 12 :el 5} " "]
   [:keyword @{:bc 12 :bl 5 :ec 14 :el 5} ":a"]
   [:whitespace @{:bc 14 :bl 5 :ec 15 :el 5} " "]
   [:number @{:bc 15 :bl 5 :ec 16 :el 5} "2"]]

  (-> (j/left test-expr-zloc)
      j/node)
  # =>
  [:whitespace @{:bc 1 :bl 5 :ec 3 :el 5} "  "]

  )

(defn find-expected-expr
  [ti-zloc]
  (def after-zlocs @[])
  (var curr-zloc ti-zloc)
  (var found-comment nil)
  (var found-after nil)
  # collect zlocs to the right of the test indicator up through the
  # first non-whitespace/comment one.  if there is a
  # non-whitespace/comment one, that is the expression used to compute
  # the expected value.
  (while curr-zloc
    (set curr-zloc (j/right curr-zloc))
    (when (nil? curr-zloc)
      (break))
    #
    (match (j/node curr-zloc)
      [:comment]
      (do
        (set found-comment true)
        (break))
      #
      [:whitespace]
      (array/push after-zlocs curr-zloc)
      #
      (do
        (set found-after true)
        (array/push after-zlocs curr-zloc)
        (break))))
  #
  (cond
    (or (nil? curr-zloc)
        found-comment)
    :no-expected-expression
    # if there was a non-whitespace/comment zloc and the first zloc
    # "captured" represents eol (i.e. the first zloc to the right of
    # the test indicator), then there might be a an "expected
    # expression" that follows...
    (and found-after
         (match (j/node (first after-zlocs))
           [:whitespace _ "\n"]
           true
           [:whitespace _ "\r\n"]
           true))
    # starting on the line after the eol zloc, keep collected zlocs up
    # to (but not including) another eol zloc.  the first
    # non-whitespace zloc of the kept zlocs represents the "expected
    # expression".
    (if-let [from-next-line (drop 1 after-zlocs)
             before-eol-zloc (take-until |(match (j/node $)
                                            [:whitespace _ "\n"]
                                            true
                                            [:whitespace _ "\r\n"]
                                            true)
                                         from-next-line)
             target (->> before-eol-zloc
                         (filter |(match (j/node $)
                                    [:whitespace]
                                    false
                                    #
                                    true))
                         first)]
      target
      :no-expected-expression)
    #
    :unexpected-result))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # =>"           eol
            "  @{:a 1"         eol
            "    :b 2}"        eol
            eol
            "  )"))

  (def [ti-zloc _ _]
    (find-test-indicator (-> (j/par src)
                             j/zip-down
                             j/down)))

  (j/node ti-zloc)
  # =>
  [:comment @{:bc 3 :bl 6 :ec 7 :el 6} "# =>"]

  (def expected-expr-zloc (find-expected-expr ti-zloc))

  (j/node expected-expr-zloc)
  # =>
  [:table @{:bc 3 :bl 7 :ec 10 :el 8}
   [:keyword @{:bc 5 :bl 7 :ec 7 :el 7} ":a"]
   [:whitespace @{:bc 7 :bl 7 :ec 8 :el 7} " "]
   [:number @{:bc 8 :bl 7 :ec 9 :el 7} "1"]
   [:whitespace @{:bc 9 :bl 7 :ec 1 :el 8} "\n"]
   [:whitespace @{:bc 1 :bl 8 :ec 5 :el 8} "    "]
   [:keyword @{:bc 5 :bl 8 :ec 7 :el 8} ":b"]
   [:whitespace @{:bc 7 :bl 8 :ec 8 :el 8} " "]
   [:number @{:bc 8 :bl 8 :ec 9 :el 8} "2"]]

  (-> (j/left expected-expr-zloc)
      j/node)
  # =>
  [:whitespace @{:bc 1 :bl 7 :ec 3 :el 7} "  "]

  (def src
    (string "(comment"                eol
            eol
            "  (butlast @[:a :b :c])" eol
            "  # => @[:a :b]"         eol
            eol
            "  (butlast [:a])"        eol
            "  # => []"               eol
            eol
            ")"))

  (def [ti-zloc _ _]
    (find-test-indicator (-> (j/par src)
                             j/zip-down
                             j/down)))

  (j/node ti-zloc)
  # =>
  [:comment @{:bc 3 :bl 4 :ec 16 :el 4} "# => @[:a :b]"]

  (find-expected-expr ti-zloc)
  # =>
  :no-expected-expression

  )

(defn make-label
  [left right]
  (string ""
          (when (not (empty? left))
            left)
          (cond
            (not (empty? left))
            " =>"
            #
            (not (empty? right))
            "=>"
            #
            "")
          (when (not (empty? right))
            (string " " right))))

(comment

  (make-label "hi" "there")
  # =>
  "hi => there"

  (make-label "hi" "")
  # =>
  "hi =>"

  (make-label "" "there")
  # =>
  "=> there"

  (make-label "" "")
  # =>
  ""

  )

(defn find-exprs
  [ti-zloc]
  (def b {:in "find-exprs" :args {:ti-zloc ti-zloc}})
  # look for a test expression
  (def test-expr-zloc (find-test-expr ti-zloc))
  (case test-expr-zloc
    :no-test-expression
    (break [nil nil])
    #
    :unexpected-result
    (e/emf b "unexpected result from `find-test-expr`: %p"
           test-expr-zloc))
  # look for an expected value expression
  (def expected-expr-zloc (find-expected-expr ti-zloc))
  (case expected-expr-zloc
    :no-expected-expression
    (break [test-expr-zloc nil])
    #
    :unexpected-result
    (e/emf b "unexpected result from `find-expected-expr`: %p"
           expected-expr-zloc))
  #
  [test-expr-zloc expected-expr-zloc])

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(+ 1 1)" eol
            "# =>"    eol
            "2"))

  (def [ti-zloc _ _]
    (find-test-indicator (-> (j/par src)
                             j/zip-down)))

  (def [t-zloc e-zloc] (find-exprs ti-zloc))

  (j/gen (j/node t-zloc))
  # =>
  "(+ 1 1)"

  (j/gen (j/node e-zloc))
  # =>
  "2"

  )

(defn wrap-as-test-call
  [start-zloc end-zloc ti-line-no test-label]
  # XXX: hack - not sure if robust enough
  (def eol-str (if (= :windows (os/which)) "\r\n" "\n"))
  (-> (j/wrap start-zloc [:tuple @{}] end-zloc)
      # newline important for preserving long strings
      (j/insert-child [:whitespace @{} eol-str])
      # name of test macro
      (j/insert-child [:symbol @{} "_verify/is"])
      # for column zero convention, insert leading whitespace
      # before the beginning of the tuple (_verify/is ...)
      (j/insert-left [:whitespace @{} "  "])
      # add location info argument
      (j/append-child [:whitespace @{} " "])
      (j/append-child [:number @{} (string ti-line-no)])
      #
      (j/append-child [:whitespace @{} " "])
      (j/append-child [:string @{} test-label])))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(+ 1 1)" eol
            "# =>"    eol
            "2"))

  (def [ti-zloc _ _]
    (find-test-indicator (-> (j/par src)
                             j/zip-down)))

  (def [t-zloc e-zloc] (find-exprs ti-zloc))

  (let [left-of-t-zloc (j/left t-zloc)
        start-zloc (match (j/node left-of-t-zloc)
                     [:whitespace]
                     left-of-t-zloc
                     #
                     t-zloc)
        w-zloc (wrap-as-test-call start-zloc e-zloc "3" `""`)]
    (j/gen (j/node w-zloc)))
  # =>
  (string "(_verify/is\n"
          "(+ 1 1)\n"
          "# =>\n"
          "2 "
          "3 "
          `""`
          ")")

  )

(defn rewrite-with-tests
  [comment-zloc]
  # move into comment block
  (var curr-zloc (j/down comment-zloc))
  (var found-test nil)
  # process comment block content
  (while (not (j/end? curr-zloc))
    (def [ti-zloc label-left label-right] (find-test-indicator curr-zloc))
    (when (not ti-zloc)
      (break))
    #
    (def [test-expr-zloc expected-expr-zloc] (find-exprs ti-zloc))
    (set curr-zloc
         (if (or (nil? test-expr-zloc)
                 (nil? expected-expr-zloc))
           (j/right curr-zloc) # next
           # found a complete test, work on rewriting
           (let [left-of-te-zloc (j/left test-expr-zloc)
                 start-zloc (match (j/node left-of-te-zloc)
                              [:whitespace]
                              left-of-te-zloc
                              #
                              test-expr-zloc)
                 end-zloc expected-expr-zloc
                 # XXX: use `attrs` here?
                 ti-line-no ((get (j/node ti-zloc) 1) :bl)
                 test-label
                 (string/format `"%s"`
                                (make-label label-left label-right))]
             (set found-test true)
             (wrap-as-test-call start-zloc end-zloc
                                ti-line-no test-label)))))
  # navigate back out to top of block
  (when found-test
    # morph comment block into plain tuple -- to be unwrapped later
    (-> curr-zloc
        j/up
        j/down
        (j/replace [:whitespace @{} " "])
        # begin hack to prevent trailing whitespace once unwrapping occurs
        j/rightmost
        (j/insert-right [:keyword @{} ":smile"])
        # end of hack
        j/up)))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # left =>"      eol
            "  @{:a 2}"        eol
            eol
            "  (+ 1 1)"        eol
            "  # => right"     eol
            "  2"              eol
            eol
            "  )"))

  (-> (j/par src)
      j/zip-down
      rewrite-with-tests
      j/root
      j/gen)
  # =>
  (string "( "                     eol
          eol
          "  (def a 1)"            eol
          eol
          "  (_verify/is"          eol
          "  (put @{} :a 2)"       eol
          "  # left =>"            eol
          `  @{:a 2} 6 "left =>")` eol
          eol
          "  (_verify/is"          eol
          "  (+ 1 1)"              eol
          "  # => right"           eol
          `  2 10 "=> right")`     eol
          eol
          "  :smile)")

  )

(defn rewrite-comment-block
  [comment-src]
  (-> (j/par comment-src)
      j/zip-down
      rewrite-with-tests
      j/root
      j/gen))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"          eol
            eol
            "  (def a 1)"       eol
            eol
            "  (put @{} :a 2)"  eol
            "  # =>"            eol
            "  @{:a 2}"         eol
            eol
            "  (+ 1 1)"         eol
            "  # left => right" eol
            "  2"               eol
            eol
            "  )"))

  (rewrite-comment-block src)
  # =>
  (string "( "                      eol
          eol
          "  (def a 1)"             eol
          eol
          "  (_verify/is"           eol
          "  (put @{} :a 2)"        eol
          "  # =>"                  eol
          `  @{:a 2} 6 "")`         eol
          eol
          "  (_verify/is"           eol
          "  (+ 1 1)"               eol
          "  # left => right"       eol
          `  2 10 "left => right")` eol
          eol
          "  :smile)")

  )

(defn rewrite-comments-with
  [src xform-fn]
  (var changed nil)
  # XXX: hack - not sure if robust enough
  (def eol-str (if (= :windows (os/which)) "\r\n" "\n"))
  (var curr-zloc
    (-> (j/par src)
        j/zip-down
        # XXX: leading newline is a hack to prevent very first thing
        #      from being a comment block
        (j/insert-left [:whitespace @{} eol-str])
        # XXX: once the newline is inserted, need to move to it
        j/left))
  #
  (while (not (j/end? curr-zloc))
    # try to find a top-level comment block
    (if-let [comment-zloc
             (j/right-until curr-zloc
                            |(match (j/node $)
                               [:tuple _ [:symbol _ "comment"]]
                               true))]
      # may be rewrite the located top-level comment block
      (set curr-zloc
           (if-let [rewritten-zloc
                    (xform-fn comment-zloc)]
             (do
               (set changed true)
               (j/unwrap rewritten-zloc))
             comment-zloc))
      (break)))
  #
  (when changed
    (-> curr-zloc
        j/root
        j/gen)))

(defn rewrite
  [src]
  (rewrite-comments-with src rewrite-with-tests))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string `(require "json")` eol
            eol
            "(defn my-fn"      eol
            "  [x]"            eol
            "  (+ x 1))"       eol
            eol
            "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # =>"           eol
            "  @{:a 2}"        eol
            eol
            "  (my-fn 1)"      eol
            "  # =>"           eol
            "  2"              eol
            eol
            "  )"              eol
            eol
            "(defn your-fn"    eol
            "  [y]"            eol
            "  (* y y))"       eol
            eol
            "(comment"         eol
            eol
            "  (your-fn 3)"    eol
            "  # =>"           eol
            "  9"              eol
            eol
            "  (def b 1)"      eol
            eol
            "  (+ b 1)"        eol
            "  # =>"           eol
            "  2"              eol
            eol
            "  (def c 2)"      eol
            eol
            "  )"              eol
            ))

  (rewrite src)
  # =>
  (string eol
          `(require "json")`     eol
          eol
          "(defn my-fn"          eol
          "  [x]"                eol
          "  (+ x 1))"           eol
          eol
          " "                    eol
          eol
          "  (def a 1)"          eol
          eol
          "  (_verify/is"        eol
          "  (put @{} :a 2)"     eol
          "  # =>"               eol
          `  @{:a 2} 12 "")`     eol
          eol
          "  (_verify/is"        eol
          "  (my-fn 1)"          eol
          "  # =>"               eol
          `  2 16 "")`           eol
          eol
          "  :smile"             eol
          eol
          "(defn your-fn"        eol
          "  [y]"                eol
          "  (* y y))"           eol
          eol
          " "                    eol
          eol
          "  (_verify/is"        eol
          "  (your-fn 3)"        eol
          "  # =>"               eol
          `  9 28 "")`           eol
          eol
          "  (def b 1)"          eol
          eol
          "  (_verify/is"        eol
          "  (+ b 1)"            eol
          "  # =>"               eol
          `  2 34 "")`           eol
          eol
          "  (def c 2)"          eol
          eol
          "  :smile"             eol)

  )

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  # https://github.com/sogaiu/judge-gen/issues/1
  (def src
    (string "(comment"        eol
            eol
            "  (-> ``"        eol
            "      123456789" eol
            "      ``"        eol
            "      length)"   eol
            "  # =>"          eol
            "  9"             eol
            eol
            "  (->"           eol
            "    ``"          eol
            "    123456789"   eol
            "    ``"          eol
            "    length)"     eol
            "  # =>"          eol
            "  9"             eol
            eol
            "  )"))

  (rewrite src)
  # =>
  (string eol
          " "               eol
          eol
          "  (_verify/is"   eol
          "  (-> ``"        eol
          "      123456789" eol
          "      ``"        eol
          "      length)"   eol
          "  # =>"          eol
          `  9 7 "")`       eol
          eol
          "  (_verify/is"   eol
          "  (->"           eol
          "    ``"          eol
          "    123456789"   eol
          "    ``"          eol
          "    length)"     eol
          "  # =>"          eol
          `  9 15 "")`      eol
          eol
          "  :smile")

  )

(defn rewrite-as-test-file
  [src]
  (when (not (empty? src))
    (when-let [rewritten (rewrite src)]
      # XXX: hack - not sure if robust enough
      (def eol-str (if (= :windows (os/which)) "\r\n" "\n"))
      (string v/as-string
              eol-str
              "(_verify/start-tests)"
              eol-str
              rewritten
              eol-str
              "(_verify/end-tests)"
              eol-str
              "(_verify/report)"
              eol-str))))

(defn rewrite-with-only-test-exprs
  [comment-zloc]
  # move into comment block
  (var curr-zloc (j/down comment-zloc))
  (var found-test nil)
  # process comment block content
  (while (not (j/end? curr-zloc))
    (def [ti-zloc label-left label-right] (find-test-indicator curr-zloc))
    (when (not ti-zloc)
      (break))
    #
    (def [test-expr-zloc expected-expr-zloc] (find-exprs ti-zloc))
    (set curr-zloc
         (if (or (nil? test-expr-zloc)
                 (nil? expected-expr-zloc))
           (j/right curr-zloc) # next
           # found a complete test, work on rewriting
           (let [eol-str (if (= :windows (os/which)) "\r\n" "\n")]
             (set found-test true)
             (-> (j/wrap expected-expr-zloc [:tuple @{}])
                 (j/insert-child [:whitespace @{} " "])
                 (j/insert-child [:symbol @{} "comment"]))))))
  # navigate back out to top of block
  (when found-test
    # morph comment block into plain tuple -- to be unwrapped later
    (-> curr-zloc
        j/up
        j/down
        (j/replace [:whitespace @{} " "])
        # begin hack to prevent trailing whitespace once unwrapping occurs
        j/rightmost
        (j/insert-right [:keyword @{} ":smile"])
        # end of hack
        j/up)))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # left =>"      eol
            "  @{:a 2}"        eol
            eol
            "  (+ 1 1)"        eol
            "  # => right"     eol
            "  2"              eol
            eol
            "  )"))

  (-> (j/par src)
      j/zip-down
      rewrite-with-only-test-exprs
      j/root
      j/gen)
  # =>
  (string "( "                     eol
          eol
          "  (def a 1)"            eol
          eol
          "  (put @{} :a 2)"       eol
          "  # left =>"            eol
          "  (comment @{:a 2})"    eol
          eol
          "  (+ 1 1)"              eol
          "  # => right"           eol
          "  (comment 2)"          eol
          eol
          "  :smile)")

  )

(defn rewrite-to-lint
  [src]
  (rewrite-comments-with src rewrite-with-only-test-exprs))

# XXX: rewrite-comments-with adds a leading newline for processing
#      purposes (see its source), but this causes all line numbers to
#      be off by one.  to make the line numbers match up, the leading
#      newline is removed.  it's nicer for the line numbers in the
#      rewritten source to match up with the original source because
#      linting messages can mention specific line numbers.
(defn rewrite-as-file-to-lint
  [src]
  (when (not (empty? src))
    (when-let [to-lint-src (rewrite-to-lint src)
               nl-idx (string/find "\n" to-lint-src)]
      # to make the line numbers match the original source
      (string/slice to-lint-src (inc nl-idx)))))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"         eol
            eol
            "  (def a 1)"      eol
            eol
            "  (put @{} :a 2)" eol
            "  # left =>"      eol
            "  @{:a 2}"        eol
            eol
            "  (+ 1 1)"        eol
            "  # => right"     eol
            "  2"              eol
            eol
            "  )"))

  (rewrite-as-file-to-lint src)
  # =>
  (string " " eol
          eol
          "  (def a 1)"         eol
          eol
          "  (put @{} :a 2)"    eol
          "  # left =>"         eol
          "  (comment @{:a 2})" eol
          eol
          "  (+ 1 1)"           eol
          "  # => right"        eol
          "  (comment 2)"       eol
          eol
          "  :smile")

  )

(defn patch-zloc
  [a-zloc update-info]
  (def b {:in "patch-zloc" :args {:a-zloc a-zloc :update-info update-info}})
  (var zloc a-zloc)
  (var ok? true)
  (each [line value] update-info
    (when (not zloc)
      (break))
    #
    (def ti-zloc
      (j/search-from zloc
                     |(when-let [node (j/node $)
                                 [n-type {:bl bl} _] node]
                        (and (= :comment n-type)
                             (= bl line)))))
    (when (not ti-zloc)
      (e/emf b "failed to find test indicator at line: %d" line))
    #
    (def ee-zloc (find-expected-expr ti-zloc))
    (def new-node
      (try (-> (j/par value)
               j/zip-down
               j/node)
        ([e] (e/emf (merge b {:e-via-try e})
                    "failed to create node for value: %n" value))))
    # patch with value
    (def new-zloc (j/replace ee-zloc new-node))
    (when (not new-zloc)
      (e/emf b "failed to replace with new node: %n" new-node))
    #
    (set zloc new-zloc))
  #
  (when ok? zloc))

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (string "(comment"  eol
            eol
            "  (+ 1 2)" eol
            "  # =>"    eol
            "  0"       eol
            eol
            "  )"))

  (def zloc (-> src j/par j/zip-down))

  (-> (patch-zloc zloc @[[4 "3"]])
      j/root
      j/gen)
  # =>
  (string "(comment"  eol
          eol
          "  (+ 1 2)" eol
          "  # =>"    eol
          "  3"       eol
          eol
          "  )")

  )

(defn patch
  [input update-info &opt output]
  (def b {:in "patch" :args {:input input :update-info update-info
                             :output output}})
  (default output (if (string? input) input @""))
  (def src (cond (string? input)
                 (slurp input)
                 #
                 (buffer? input)
                 input
                 #
                 (e/emf b "unexpected type for input: %n" input)))
  (when (empty? src)
    (e/emf b "no content for input: %n" input))
  # prepare and patch
  (def zloc
    (try (-> src j/par j/zip-down)
      ([e] (e/emf (merge b {:e-via-try e})
                  "failed to create zipper for: %n" input))))
  (def new-zloc (patch-zloc zloc update-info))
  (when (not new-zloc)
    (break nil))
  #
  (def new-src
    (try
      (-> new-zloc j/root j/gen)
      ([e]
        (e/emf (merge b {:e-via-try e})
               "failed to create src from: %n" (j/node new-zloc)))))
  (when (not new-src)
    (e/emf b "unexpected falsy value for new-src"))
  #
  (cond (buffer? output)
        (buffer/blit output new-src)
        #
        (string? output)
        (spit output new-src)
        #
        (e/emf b "unexpected value for output: %n" output))
  #
  output)

(comment

  (def eol (if (= :windows (os/which)) "\r\n" "\n"))

  (def src
    (buffer "(comment"             eol
            eol
            "  (+ 1 (- 2"          eol
            "          (+ 1 2))) " eol
            "  # =>"               eol
            "  3"                  eol
            eol
            "  )"))

  (patch src @[[5 "0"]] @"")
  # =>
  (buffer "(comment"             eol
          eol
          "  (+ 1 (- 2"          eol
          "          (+ 1 2))) " eol
          "  # =>"               eol
          "  0"                  eol
          eol
          "  )")

  )

