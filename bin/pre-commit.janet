#! /usr/bin/env janet

(use ./sh-dsl)

(prin "running jell...") (flush)
(def jell-exit ($ janet ./bin/jell))
(assertf (zero? jell-exit)
         "jell exited: %d" jell-exit)
(print "done")

(print "running niche...")
(def niche-exit ($ janet ./bin/niche.janet))
(assertf (zero? niche-exit)
         "niche exited: %d" niche-exit)
(print "done")

(print "updating README...")
(def readme-update-ext ($ janet quiche.janet -h > README))
(assertf (zero? readme-update-ext)
         "updating README exited: %d" readme-update-ext)
(print "done")

