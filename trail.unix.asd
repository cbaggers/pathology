;;;; trail.asd

(asdf:defsystem #:trail.unix
  :description "Use trail's route type to define a simple unix path type"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:trail)
  :components ((:file "unix/package")
               (:file "unix/unix-path")))
