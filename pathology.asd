;;;; pathology.route.asd

(asdf:defsystem #:pathology
  :description "An attempt at very simple path types"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (#:pathology.route #:pathology.route.unix)
  :components ((:file "pathology/package")
               (:file "pathology/base")))
