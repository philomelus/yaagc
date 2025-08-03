(in-package #:yaagc)

(defun asset (path module)
  (namestring (asdf:system-relative-pathname "yaagc" (concatenate 'string "assets/"
                                                                  (concatenate 'string module "/")
                                                                  path))))
