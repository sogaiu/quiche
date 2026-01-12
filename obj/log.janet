# :w - warn
# :e - error
# :i - info
# :o - output

(def l/d-table
  {:w eprin
   :e eprin
   :i eprin
   :o prin})

(defn l/note
  [flavor & args]
  (def disp-table (dyn :d-table l/d-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

(def l/df-table
  {:w eprinf
   :e eprinf
   :i eprinf
   :o prinf})

(defn l/notef
  [flavor & args]
  (def disp-table (dyn :df-table l/df-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

(def l/dn-table
  {:w eprint
   :e eprint
   :i eprint
   :o print})

(defn l/noten
  [flavor & args]
  (def disp-table (dyn :dn-table l/dn-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

(def l/dnf-table
  {:w eprintf
   :e eprintf
   :i eprintf
   :o printf})

(defn l/notenf
  [flavor & args]
  (def disp-table (dyn :dnf-table l/dnf-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

########################################################################

(def l/ignore-table
  {:w (fn :w [& _] nil)
   :e (fn :e [& _] nil)
   :i (fn :i [& _] nil)
   :o (fn :o [& _] nil)})

(defn l/set-d-tables!
  [{:d d :df df :dn dn :dnf dnf}]
  (default d l/d-table)
  (default df l/df-table)
  (default dn l/dn-table)
  (default dnf l/dnf-table)
  (setdyn :d-table d)
  (setdyn :df-table df)
  (setdyn :dn-table dn)
  (setdyn :dnf-table dnf))

(defn l/clear-d-tables!
  []
  (l/set-d-tables! {:d l/ignore-table
                  :df l/ignore-table
                  :dn l/ignore-table
                  :dnf l/ignore-table}))

(defn l/reset-d-tables!
  []
  (l/set-d-tables! {}))

