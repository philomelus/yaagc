(in-package #:yaagc-assets)

(defun asset (path module)
  (namestring (asdf:system-relative-pathname "yaagc" (concatenate 'string "assets/"
                                                                  (concatenate 'string (lowercase (symbol-name module)) "/")
                                                                  path))))
