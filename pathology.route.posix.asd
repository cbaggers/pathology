;;;; pathology.route.asd

(asdf:defsystem #:pathology.route.posix
  :description "Use pathology.route's route type to define a simple posix path type"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:pathology.route)
  :components ((:file "pathology.route.posix/package")
               (:file "pathology.route.posix/posix-path")))