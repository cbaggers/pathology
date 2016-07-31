;;;; pathology.route.asd

(asdf:defsystem #:pathology.route
  :description "Provides a route type which is a series of utf8 tokens"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :components ((:file "pathology.route/package")
               (:file "pathology.route/route")
	       (:file "pathology.route/flavor")))
