(import ./errors :as e)

(def conf-file ".niche.jdn")

(defn parse-conf-file
  [conf-file]
  (def b {:in "parse-conf-file" :args {:conf-file conf-file}})
  #
  (let [src (try (slurp conf-file)
              ([e] (e/emf (merge b {:e-via-try e})
                          "failed to slurp: %s" conf-file)))
        cnf (try (parse src)
              ([e] (e/emf (merge b {:e-via-try e})
                          "failed to parse: %s" conf-file)))]
    (when (not cnf)
      (e/emf b "failed to load: %s" conf-file))
    #
    (when (not (dictionary? cnf))
      (e/emf b "expected dictionary in conf, got: %s" (type cnf)))
    #
    [(array ;(get cnf :includes @[]))
     (array ;(get cnf :excludes @[]))]))

