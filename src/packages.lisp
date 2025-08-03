(defpackage :yaagc-assets
  (:use #:cl)
  (:export #:asset))

(defpackage :yaagc-asteroids
  (:use #:cl)
  (:export #:asteroids-main))

(defpackage :yaagc-blastem
  (:use #:cl)
  (:export #:blastem-main))

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
                #:manager)
  (:import-from #:yaagc-asset
                #:asset)
  (:import-from #:yaagc-asteroids
                #:asteroids-main)
  (:import-from #:yaagc-blastem
                #:blastem-main)
  (:export #:main))

