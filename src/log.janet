# :w - warn
# :e - error
# :i - info
# :o - output

(def d-table
  {:w eprin
   :e eprin
   :i eprin
   :o prin})

(defn note
  [flavor & args]
  (def disp-table (dyn :d-table d-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

(def df-table
  {:w eprinf
   :e eprinf
   :i eprinf
   :o prinf})

(defn notef
  [flavor & args]
  (def disp-table (dyn :df-table df-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

(def dn-table
  {:w eprint
   :e eprint
   :i eprint
   :o print})

(defn noten
  [flavor & args]
  (def disp-table (dyn :dn-table dn-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

(def dnf-table
  {:w eprintf
   :e eprintf
   :i eprintf
   :o printf})

(defn notenf
  [flavor & args]
  (def disp-table (dyn :dnf-table dnf-table))
  (def dispatch-fn (get disp-table flavor))
  (assertf dispatch-fn "unknown flavor: %n" flavor)
  #
  (dispatch-fn ;args))

########################################################################

(def ignore-table
  {:w (fn :w [& _] nil)
   :e (fn :e [& _] nil)
   :i (fn :i [& _] nil)
   :o (fn :o [& _] nil)})

(defn set-d-tables!
  [{:d d :df df :dn dn :dnf dnf}]
  (default d d-table)
  (default df df-table)
  (default dn dn-table)
  (default dnf dnf-table)
  (setdyn :d-table d)
  (setdyn :df-table df)
  (setdyn :dn-table dn)
  (setdyn :dnf-table dnf))

(defn clear-d-tables!
  []
  (set-d-tables! {:d ignore-table
                  :df ignore-table
                  :dn ignore-table
                  :dnf ignore-table}))

(defn reset-d-tables!
  []
  (set-d-tables! {}))

