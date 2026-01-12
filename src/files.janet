(import ./paths :as p)

(defn is-file?
  [path]
  #
  (= :file (os/stat path :mode)))

(defn find-files
  [dir &opt pred]
  (default pred identity)
  (def paths @[])
  (defn helper
    [a-dir]
    (each path (os/dir a-dir)
      (def sub-path (string a-dir p/sep path))
      (case (os/stat sub-path :mode)
        :directory
        (when (not= path ".git")
          (helper sub-path))
        #
        :file
        (when (pred sub-path)
          (array/push paths sub-path)))))
  (helper dir)
  paths)

(comment

  (find-files "." |(string/has-suffix? ".janet" $))

  )

(defn has-janet-shebang?
  [path]
  (with [f (file/open path)]
    (def first-line (file/read f :line))
    (when first-line
      (and (string/find "env" first-line)
           (string/find "janet" first-line)))))

(defn collect-paths
  [includes &opt pred]
  (default pred identity)
  (def filepaths @[])
  # collect file and directory paths
  (each thing includes
    (def apath (p/clean-end-of-path thing p/sep))
    (def mode (os/stat apath :mode))
    # XXX: should :link be supported?
    (cond
      (= :file mode)
      (array/push filepaths apath)
      #
      (= :directory mode)
      (array/concat filepaths (find-files apath pred))))
  #
  filepaths)

