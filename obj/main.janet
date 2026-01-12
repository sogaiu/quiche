(import ./args :prefix "")
(import ./commands :prefix "")
(import ./errors :prefix "")
(import ./files :prefix "")
(import ./log :prefix "")
(import ./output :prefix "")

###########################################################################

(def version "DEVEL")

(def usage
  ``
  Usage: quiche [<file-or-dir>...]
         quiche [-h|--help] [-v|--version]

  Quaintly Update Inadequate Comment-Hidden Expressions

  Parameters:

    <file-or-dir>          path to file or directory

  Options:

    -h, --help             show this output
    -v, --version          show version information

  Configuration:

    .niche.jdn              configuration file [1]

  Examples:

    1. Update comment-hidden expressions in `src/`
       directory:

       $ quiche src

    2. Run using the configuration file via direct
       invocation:

       $ quiche

  Example `.niche.jdn` content:

    {# what to work on - file and dir paths
     :includes ["src" "bin/my-script"]
     # what to skip - file paths only
     :excludes ["src/sample.janet"]}

  ---

  [1] quiche makes use of niche's configuration
      file.  See:

        https://github.com/sogaiu/niche

      for more information about niche.
  ``)

########################################################################

(defn main
  [& args]
  (def start-time (os/clock))
  #
  (def opts (a/parse-args (drop 1 args)))
  #
  (when (get opts :show-help)
    (l/noten :o usage)
    (os/exit 0))
  #
  (when (get opts :show-version)
    (l/noten :o version)
    (os/exit 0))
  #
  (def src-paths
    (f/collect-paths (get opts :includes)
                     |(or (string/has-suffix? ".janet" $)
                          (f/has-janet-shebang? $))))
  (when (get opts :raw)
    (l/clear-d-tables!))
  # 0 - successful testing / updating
  # 1 - at least one test failure
  # 2 - caught error
  (def [exit-code test-results]
    (try
      (c/make-run-update src-paths opts)
      ([e f]
        (l/noten :e)
        (if (dictionary? e)
          (e/show e)
          (debug/stacktrace f e "internal "))
        (l/noten :e "Processing halted.")
        [2 @[]])))
  #
  (if (get opts :raw)
    (print (o/color-form test-results))
    (l/notenf :i "Total processing time was %.02f secs."
              (- (os/clock) start-time)))
  #
  (when (not (get opts :no-exit))
    (os/exit exit-code)))

