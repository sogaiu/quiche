(import ./errors :prefix "")

(def s/conf-file ".niche.jdn")

(defn s/parse-conf-file
  [s/conf-file]
  (def b {:in "parse-conf-file" :args {:conf-file s/conf-file}})
  #
  (let [src (try (slurp s/conf-file)
              ([e] (e/emf (merge b {:e-via-try e})
                          "failed to slurp: %s" s/conf-file)))
        cnf (try (parse src)
              ([e] (e/emf (merge b {:e-via-try e})
                          "failed to parse: %s" s/conf-file)))]
    (when (not cnf)
      (e/emf b "failed to load: %s" s/conf-file))
    #
    (when (not (dictionary? cnf))
      (e/emf b "expected dictionary in conf, got: %s" (type cnf)))
    #
    [(array ;(get cnf :includes @[]))
     (array ;(get cnf :excludes @[]))]))

