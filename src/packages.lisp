(defpackage :yaagc
  (:use #:cl)
  (:import-from #:cffi
                #:foreign-alloc
                #:foreign-enum-value
                #:foreign-free
                #:foreign-pointer
                #:foreign-slot-value
                #:null-pointer
                #:null-pointer-p)
  (:import-from #:cl-yag
                #:window
                #:active-text
                #:column-layout
                #:manager)
  (:export #:+KEY-DOWN+
           #:+KEY-LEFT+
           #:+KEY-MAX+
           #:+KEY-RIGHT+
           #:+KEY-UP+
           #:+KEY-X+
           #:+KEY-ESC+
           #:+KEY-B+
           #:asset
           #:between
           #:between-f
           #:color2assoc
           #:color2list
           #:color-a
           #:color-b
           #:color-g
           #:color-inverse
           #:color-r
           #:must-init
           #:rect-collide
           #:main))

(defpackage :yaagc-asteroids
  (:use #:cl)
  (:import-from #:cffi
                #:foreign-alloc
                #:foreign-enum-value
                #:foreign-free
                #:foreign-pointer
                #:foreign-slot-value
                #:null-pointer
                #:null-pointer-p)
  (:import-from #:yaagc
                #:+KEY-MAX+
                #:+KEY-DOWN+
                #:+KEY-LEFT+
                #:+KEY-RIGHT+
                #:+KEY-UP+
                #:+KEY-X+
                #:+KEY-ESC+
                #:+KEY-B+
                #:asset)
  (:export #:asteroids-main))

(defpackage :yaagc-blastem
  (:use #:cl)
  (:import-from #:cffi
                #:foreign-alloc
                #:foreign-enum-value
                #:foreign-free
                #:foreign-pointer
                #:foreign-slot-value
                #:null-pointer
                #:null-pointer-p)
  (:import-from #:yaagc
                #:+KEY-MAX+
                #:+KEY-DOWN+
                #:+KEY-LEFT+
                #:+KEY-RIGHT+
                #:+KEY-UP+
                #:+KEY-X+
                #:+KEY-ESC+
                #:+KEY-B+
                #:asset
                #:between
                #:between-f
                #:must-init
                #:rect-collide
                )
  (:export #:blastem-main))

