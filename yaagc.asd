(defsystem "yaagc"
  :author "Russell E. Gibson <russg@rnstech.com>"
  :version "0.0.1"
  :license "MIT"
  :description "Ye Another Arcade Game Collection"
  :homepage "https://github.com/philomelus/yaagc"
  :bug-tracker "https://github.com/philomelus/yaagc/issues"
  :source-control (:git "https://github.com/philomelus/yaagc")

  ;; Dependencies.
  :depends-on ("cl-liballegro"
               "cl-yag"
               "documentation-utils")

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "yaagc")
                             (:file "yaagc-docs")

                             (:module "assets"
                              :components
                              ((:file "assets")))
                             
                             (:module "asteroids"
                              :components
                              ((:file "asteroids")
                               (:file "asteroids-docs")))
                             
                             (:module "blastem"
                              :components
                              ((:file "blastem")
                               (:file "blastem-docs"))))))
  
  :around-compile
  (lambda (next)
    ;; (proclaim '(optimize (compilation-speed 0) (debug 3) (safety 3) (space 0) (speed 0)))    
    (proclaim '(optimize (debug 3) (safety 3)))    
    (funcall next)))

