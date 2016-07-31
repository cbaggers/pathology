;;;; pathology.route.asd

(asdf:defsystem #:pathology.route.unix
  :description "Use pathology.route's route type to define a simple unix path type"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:pathology.route)
  :components ((:file "pathology.route.unix/package")
               (:file "pathology.route.unix/unix-path")))
